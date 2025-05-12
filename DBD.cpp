#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"


#include <llvm/Support/InitLLVM.h>
#include "clang/AST/AST.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Driver/Options.h"

#include <sstream>

#include "blocks/block.h"
#include "blocks/c_code_generator.h"
#include "blocks/expr.h"
#include "blocks/stmt.h"
#include "blocks/var.h"


static llvm::cl::OptionCategory
        DynamicByDefaultCategory("dynamic-by-default options");

using namespace clang;
using namespace llvm;

/* Resuable "block"s */

block::scalar_type::Ptr int_type = nullptr;
block::scalar_type::Ptr char_type = nullptr;
block::scalar_type::Ptr float_type = nullptr;
block::scalar_type::Ptr void_type = nullptr;
block::int_const::Ptr const_one = nullptr;
block::int_const::Ptr const_zero = nullptr;
/*--- */

class DynamicByDefaultVisitor
    : public RecursiveASTVisitor<DynamicByDefaultVisitor>
{
private:
        ASTContext * context;
        std::vector<block::block::Ptr> & new_decls;

public:
        DynamicByDefaultVisitor(
                ASTContext * context,
                std::vector<block::block::Ptr> & new_decls)
            : context(context), new_decls(new_decls)
        {
        }

        block::stmt_block::Ptr promote_to_body(Stmt * s)
        {
                auto bs = std::make_shared<block::stmt_block>();
                std::vector<block::stmt::Ptr> stmts;
                lower_stmt_into(s, stmts);
                if (stmts.size() != 1)
                {
                        bs->stmts = stmts;
                        return bs;
                }
                if (block::isa<block::stmt_block>(stmts.back()))
                        return block::to<block::stmt_block>(stmts.back());
                bs->stmts = stmts;
                return bs;
        }

        block::type::Ptr lower_type(QualType type)
        {
                auto t = type.getTypePtr();
                if (type->isVoidType())
                        return void_type;
                else if (auto bit = dyn_cast<BuiltinType>(t))
                {
                        switch (bit->getKind())
                        {
                                case BuiltinType::Char_U:
                                case BuiltinType::Char_S:
                                        return char_type;
                                case BuiltinType::UInt:
                                case BuiltinType::Int:
                                        return int_type;
                                case BuiltinType::Float:
                                        return float_type;
                                default:
                                        bit->dump();
                                        llvm_unreachable(
                                                "Cannot handle builtin type");
                        }

                        if (bit->isInteger())
                                return int_type;
                        if (bit->isFloatingPoint())
                                return float_type;
                }
                else if (
                        const clang::PointerType * pt
                        = dyn_cast<clang::PointerType>(t))
                {
                        auto bpt = std::make_shared<block::pointer_type>();
                        bpt->pointee_type = lower_type(pt->getPointeeType());
                        return bpt;
                }
                else if (
                        const ConstantArrayType * cat
                        = dyn_cast<const ConstantArrayType>(t))
                {
                        auto bat = std::make_shared<block::array_type>();
                        bat->element_type = lower_type(cat->getElementType());
                        bat->size = cat->getSize().getSExtValue();
                        return bat;
                }
                else if (
                        const DecayedType * dt = dyn_cast<const DecayedType>(t))
                {
                        return lower_type(dt->getDecayedType());
                }
                type.dump();
                llvm_unreachable("Can't resolve type");
        }

        block::type::Ptr wrap_type(block::type::Ptr type)
        {
                auto bt = std::make_shared<block::builder_var_type>();
                bt->closure_type = type;
                bt->builder_var_type_id = block::builder_var_type::DYN_VAR;
                return bt;
        }

        block::var::Ptr lower_function_arg(ParmVarDecl * p)
        {
                block::var::Ptr v = std::make_shared<block::var>();
                v->var_name = p->getNameAsString();
                v->var_type = lower_type(p->getType());
                return v;
        }


        block::expr::Ptr
        create_compound_expr(BinaryOperator * bo, block::binary_expr::Ptr bbe)
        {
                // Lowered as a = a <bbe> b
                auto e1 = lower_expr(bo->getLHS());
                auto m = bbe;
                m->expr1 = e1;
                m->expr2 = lower_expr(bo->getRHS());
                auto a = std::make_shared<block::assign_expr>();
                a->var1 = e1;
                a->expr1 = m;
                return a;
        }

        block::expr::Ptr lower_expr(Expr * e)
        {
                if (e == nullptr)
                        llvm_unreachable("Expr cannot be null?");
                if (ParenExpr * pe = dyn_cast<ParenExpr>(e))
                {
                        return lower_expr(pe->getSubExpr());
                }
                else if (IntegerLiteral * il = dyn_cast<IntegerLiteral>(e))
                {
                        auto ic = std::make_shared<block::int_const>();
                        ic->value = il->getValue().getSExtValue();
                        ic->is_64bit = false;
                        return ic;
                }
                else if (CharacterLiteral * cl = dyn_cast<CharacterLiteral>(e))
                {
                        auto ic = std::make_shared<block::int_const>();
                        ic->value = cl->getValue();
                        ic->is_64bit = false;
                        return ic;
                }
                else if (BinaryOperator * bo = dyn_cast<BinaryOperator>(e))
                {
                        if (bo->getOpcode() == BO_Assign)
                        {
                                auto a = std::make_shared<block::assign_expr>();
                                a->var1 = lower_expr(bo->getLHS());
                                a->expr1 = lower_expr(bo->getRHS());
                                return a;
                        }
                        else
                        {
                                // Handle the regular binary operators
                                block::binary_expr::Ptr be = nullptr;
                                switch (bo->getOpcode())
                                {
                                        case BO_Add:
                                                be = std::make_shared<
                                                        block::plus_expr>();
                                                break;
                                        case BO_LT:
                                                be = std::make_shared<
                                                        block::lt_expr>();
                                                break;
                                        case BO_GT:
                                                be = std::make_shared<
                                                        block::gt_expr>();
                                                break;
                                        case BO_LE:
                                                be = std::make_shared<
                                                        block::lte_expr>();
                                                break;
                                        case BO_GE:
                                                be = std::make_shared<
                                                        block::gte_expr>();
                                                break;
                                        case BO_LAnd:
                                                be = std::make_shared<
                                                        block::and_expr>();
                                                break;
                                        case BO_LOr:
                                                be = std::make_shared<
                                                        block::or_expr>();
                                                break;
                                        case BO_NE:
                                                be = std::make_shared<
                                                        block::ne_expr>();
                                                break;
                                        case BO_EQ:
                                                be = std::make_shared<
                                                        block::equals_expr>();
                                                break;
                                        case BO_Rem:
                                                be = std::make_shared<
                                                        block::mod_expr>();
                                                break;
                                        case BO_Mul:
                                                be = std::make_shared<
                                                        block::mul_expr>();
                                                break;
                                        case BO_Div:
                                                be = std::make_shared<
                                                        block::div_expr>();
                                                break;
                                        case BO_Sub:
                                                be = std::make_shared<
                                                        block::minus_expr>();
                                                break;
                                        case BO_Shl:
                                                be = std::make_shared<
                                                        block::lshift_expr>();
                                                break;
                                        case BO_Shr:
                                                be = std::make_shared<
                                                        block::rshift_expr>();
                                                break;
                                        case BO_And:
                                                be = std::make_shared<
                                                        block::bitwise_and_expr>();
                                                break;
                                        case BO_Xor:
                                                be = std::make_shared<
                                                        block::bitwise_xor_expr>();
                                                break;
                                        case BO_Or:
                                                be = std::make_shared<
                                                        block::bitwise_or_expr>();
                                                break;
                                        // Operations like MulAssign need to be
                                        // handled separately
                                        case BO_MulAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::mul_expr>());
                                                break;
                                        case BO_DivAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::div_expr>());
                                                break;
                                        case BO_RemAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::mod_expr>());
                                                break;
                                        case BO_AddAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::plus_expr>());
                                                break;
                                        case BO_SubAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::minus_expr>());
                                                break;
                                        case BO_ShlAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::lshift_expr>());
                                                break;
                                        case BO_ShrAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::rshift_expr>());
                                                break;
                                        case BO_AndAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::bitwise_and_expr>());
                                                break;
                                        case BO_XorAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::bitwise_xor_expr>());
                                                break;
                                        case BO_OrAssign:
                                                return create_compound_expr(
                                                        bo,
                                                        std::make_shared<
                                                                block::bitwise_or_expr>());
                                                break;
                                        default:
                                                bo->dump();
                                                llvm_unreachable(
                                                        "Cannot handle binary "
                                                        "operator");
                                                break;
                                }
                                be->expr1 = lower_expr(bo->getLHS());
                                be->expr2 = lower_expr(bo->getRHS());
                                return be;
                        }
                }
                else if (UnaryOperator * uo = dyn_cast<UnaryOperator>(e))
                {
                        // Most unary operators can be handled as unary
                        // operators, some need special handling as binary
                        // operators and expressions

                        if (uo->getOpcode() == UO_PostInc)
                        {
                                // Post inc is realized as (a = a + 1) - 1
                                // TODO: consider adding a postfix ++ in BuildIt
                                auto e1 = lower_expr(uo->getSubExpr());
                                auto p = std::make_shared<block::plus_expr>();
                                p->expr1 = e1;
                                p->expr2 = const_one;
                                auto a = std::make_shared<block::assign_expr>();
                                a->var1 = e1;
                                a->expr1 = p;
                                auto m = std::make_shared<block::minus_expr>();
                                m->expr1 = a;
                                m->expr2 = const_one;
                                return m;
                        }
                        else if (uo->getOpcode() == UO_PostDec)
                        {
                                auto e1 = lower_expr(uo->getSubExpr());
                                auto p = std::make_shared<block::minus_expr>();
                                p->expr1 = e1;
                                p->expr2 = const_one;
                                auto a = std::make_shared<block::assign_expr>();
                                a->var1 = e1;
                                a->expr1 = p;
                                auto m = std::make_shared<block::plus_expr>();
                                m->expr1 = a;
                                m->expr2 = const_one;
                                return m;
                        }
                        else if (uo->getOpcode() == UO_Deref)
                        {
                                auto sq = std::make_shared<
                                        block::sq_bkt_expr>();
                                sq->var_expr = lower_expr(uo->getSubExpr());
                                sq->index = const_zero;
                                return sq;
                        }
                        else
                        {
                                // General unary operators
                                block::unary_expr::Ptr ue;
                                switch (uo->getOpcode())
                                {
                                        case UO_AddrOf:
                                                ue = std::make_shared<
                                                        block::addr_of_expr>();
                                                break;
                                        default:
                                                uo->dump();
                                                llvm_unreachable(
                                                        "Cannot handle unary "
                                                        "operator");
                                                break;
                                }
                                ue->expr1 = lower_expr(uo->getSubExpr());
                                return ue;
                        }
                }
                else if (CastExpr * ice = dyn_cast<CastExpr>(e))
                {
                        if (ImplicitCastExpr * ice
                            = dyn_cast<ImplicitCastExpr>(e))
                        {
                                // All implicit casts can actually be lowered as
                                // it is, There is special cast with void*
                                // ofcourse due to how C and C++ treat them
                                // TODO: handle void* to T* casts
                                return lower_expr(ice->getSubExpr());
                        }
                        if (ice->getCastKind() == CK_LValueToRValue)
                        {
                                return lower_expr(ice->getSubExpr());
                        }
                        else
                        {
                                ice->dump();
                                llvm_unreachable(
                                        "Cannot handle cast operation");
                        }
                }
                else if (DeclRefExpr * dr = dyn_cast<DeclRefExpr>(e))
                {
                        ValueDecl * vd = dr->getDecl();
                        auto v = std::make_shared<block::var>();
                        v->var_name = vd->getNameAsString();
                        // type here doesn't matter, but cannot be left null
                        v->var_type = int_type;
                        auto ve = std::make_shared<block::var_expr>();
                        ve->var1 = v;
                        return ve;
                }
                else if (
                        ArraySubscriptExpr * ase
                        = dyn_cast<ArraySubscriptExpr>(e))
                {
                        auto sbe = std::make_shared<block::sq_bkt_expr>();
                        sbe->var_expr = lower_expr(ase->getLHS());
                        sbe->index = lower_expr(ase->getRHS());
                        return sbe;
                }
                else if (CallExpr * ce = dyn_cast<CallExpr>(e))
                {
                        auto bce
                                = std::make_shared<block::function_call_expr>();
                        bce->expr1 = lower_expr(ce->getCallee());
                        for (auto arg : ce->arguments())
                                bce->args.push_back(lower_expr(arg));
                        return bce;
                }
                else if (InitListExpr * ile = dyn_cast<InitListExpr>(e))
                {
                        auto bile = std::make_shared<
                                block::initializer_list_expr>();
                        for (auto in : ile->inits())
                                bile->elems.push_back(lower_expr(in));
                        return bile;
                }
                else if (
                        clang::StringLiteral * sl
                        = dyn_cast<clang::StringLiteral>(e))
                {
                        auto bse = std::make_shared<block::string_const>();
                        bse->value = sl->getString().str();
                        return bse;
                }
                else
                {
                        e->dump();
                        llvm_unreachable("Cannot handle expr type");
                }
                return nullptr;
        }

        void lower_stmt_into(Stmt * s, std::vector<block::stmt::Ptr> & out)
        {
                if (CompoundStmt * cs = dyn_cast<CompoundStmt>(s))
                {
                        block::stmt_block::Ptr nb
                                = std::make_shared<block::stmt_block>();
                        for (auto s : cs->body())
                                lower_stmt_into(s, nb->stmts);
                        out.push_back(nb);
                        return;
                }
                else if (DeclStmt * ds = dyn_cast<DeclStmt>(s))
                {
                        for (auto de : ds->decls())
                        {
                                if (VarDecl * vd = dyn_cast<VarDecl>(de))
                                {
                                        // TODO figure out if we need to reuse
                                        // vars or is it okay for them to be
                                        // separate
                                        block::var::Ptr v = std::make_shared<
                                                block::var>();
                                        v->var_name = vd->getName().str();
                                        v->var_type = lower_type(vd->getType());
                                        block::decl_stmt::Ptr d
                                                = std::make_shared<
                                                        block::decl_stmt>();
                                        d->decl_var = v;
                                        if (vd->hasInit())
                                                d->init_expr = lower_expr(
                                                        vd->getInit());
                                        else
                                                d->init_expr = nullptr;
                                        out.push_back(d);
                                }
                                else
                                {
                                        de->dump();
                                        llvm_unreachable(
                                                "Cannot handle decl in "
                                                "decl_stmt");
                                }
                        }
                        return;
                }
                else if (ForStmt * fs = dyn_cast<ForStmt>(s))
                {
                        block::for_stmt::Ptr bfs
                                = std::make_shared<block::for_stmt>();
                        std::vector<block::stmt::Ptr> is;
                        lower_stmt_into(fs->getInit(), is);
                        if (is.size() != 1)
                        {
                                llvm_unreachable(
                                        "For loop can have only one stmt in "
                                        "initialization");
                        }
                        bfs->decl_stmt = is.back();
                        bfs->cond = lower_expr(fs->getCond());
                        bfs->update = lower_expr(fs->getInc());
                        bfs->body = promote_to_body(fs->getBody());
                        out.push_back(bfs);
                        return;
                }
                else if (WhileStmt * ws = dyn_cast<WhileStmt>(s))
                {
                        auto bws = std::make_shared<block::while_stmt>();
                        bws->cond = lower_expr(ws->getCond());
                        bws->body = promote_to_body(ws->getBody());
                        out.push_back(bws);
                        return;
                }
                else if (IfStmt * is = dyn_cast<IfStmt>(s))
                {
                        auto bis = std::make_shared<block::if_stmt>();
                        bis->cond = lower_expr(is->getCond());
                        if (is->getThen())
                        {
                                bis->then_stmt = promote_to_body(is->getThen());
                        }
                        else
                        {
                                bis->then_stmt
                                        = std::make_shared<block::stmt_block>();
                        }
                        if (is->getElse())
                        {
                                bis->else_stmt = promote_to_body(is->getElse());
                        }
                        else
                        {
                                bis->else_stmt
                                        = std::make_shared<block::stmt_block>();
                        }
                        out.push_back(bis);
                        return;
                }
                else if (ValueStmt * vs = dyn_cast<ValueStmt>(s))
                {
                        if (vs->getExprStmt() == nullptr)
                                return;
                        auto es = std::make_shared<block::expr_stmt>();
                        es->expr1 = lower_expr(vs->getExprStmt());
                        out.push_back(es);
                        return;
                }
                else if (ReturnStmt * rs = dyn_cast<ReturnStmt>(s))
                {
                        auto brs = std::make_shared<block::return_stmt>();
                        if (rs->getRetValue())
                        {
                                brs->return_val = lower_expr(rs->getRetValue());
                        }
                        else
                        {
                                // This is a special case for return statements
                                // in a void returning function
                                brs->return_val = nullptr;
                        }
                        out.push_back(brs);
                        return;
                }
                else
                {
                        s->dump();
                        llvm_unreachable("Cannot handle statement");
                }
                return;
        }

        bool VisitFunctionDecl(FunctionDecl * fd)
        {
                auto func_decl = std::make_shared<block::func_decl>();
                func_decl->func_name = fd->getName();
                func_decl->return_type = lower_type(fd->getReturnType());

                for (auto pi : fd->parameters())
                        func_decl->args.push_back(lower_function_arg(pi));
                if (fd->hasBody())
                {
                        func_decl->body = promote_to_body(fd->getBody());
                }
                else
                {
                        func_decl->body = std::make_shared<block::stmt_block>();
                        func_decl->setMetadata<bool>("is_decl_only", true);
                }
                if (fd->isVariadic())
                        func_decl->setMetadata<bool>("is_variadic", true);
                new_decls.push_back(func_decl);
                return true;
        }
};

class DynamicByDefaultConsumer : public ASTConsumer
{
private:
        DynamicByDefaultVisitor Visitor;
        std::vector<block::block::Ptr> new_decls;

public:
        explicit DynamicByDefaultConsumer(ASTContext * context)
            : Visitor(context, new_decls)
        {
        }
        virtual void HandleTranslationUnit(ASTContext & context) override
        {
                Visitor.TraverseDecl(context.getTranslationUnitDecl());


                // Dump all the generated decls
                std::stringstream ss;
                for (auto decl : new_decls)
                        block::c_code_generator::generate_code(decl, ss, 0);
                llvm::errs() << ss.str();
        }
};


class DynamicByDefaultAction : public ASTFrontendAction
{
public:
        virtual std::unique_ptr<ASTConsumer>
        CreateASTConsumer(CompilerInstance & CI, StringRef infile) override
        {
                return std::unique_ptr<ASTConsumer>(
                        new DynamicByDefaultConsumer(&CI.getASTContext()));
        }
};

class DynamicByDefaultFrontendActionFactory
    : public tooling::FrontendActionFactory
{
public:
        DynamicByDefaultFrontendActionFactory() { }

        std::unique_ptr<FrontendAction> create() override
        {
                return std::make_unique<DynamicByDefaultAction>();
        }
};


int main(int argc, const char * argv[])
{
        llvm::InitLLVM X(argc, argv);

        // Initialize all reusable blocks
        int_type = std::make_shared<block::scalar_type>();
        int_type->scalar_type_id = block::scalar_type::INT_TYPE;
        char_type = std::make_shared<block::scalar_type>();
        char_type->scalar_type_id = block::scalar_type::CHAR_TYPE;
        float_type = std::make_shared<block::scalar_type>();
        float_type->scalar_type_id = block::scalar_type::FLOAT_TYPE;
        void_type = std::make_shared<block::scalar_type>();
        void_type->scalar_type_id = block::scalar_type::VOID_TYPE;
        const_one = std::make_shared<block::int_const>();
        const_one->value = 1;
        const_one->is_64bit = false;
        const_zero = std::make_shared<block::int_const>();
        const_zero->value = 0;
        const_zero->is_64bit = false;

        if (argc > 1)
        {
                auto OptionsParser = tooling::CommonOptionsParser::create(
                        argc, argv, DynamicByDefaultCategory);
                if (!OptionsParser)
                        return -1;
                tooling::ClangTool Tool(
                        OptionsParser->getCompilations(),
                        OptionsParser->getSourcePathList());
                DynamicByDefaultFrontendActionFactory Factory;
                Tool.run(&Factory);
                return 0;
        }
        return -1;
}
