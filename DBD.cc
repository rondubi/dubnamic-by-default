#include "DBD.hh"

#include <llvm/Support/InitLLVM.h>

static constexpr const std::array<std::string_view, 1> make_static
        = {std::string_view("y")};
/* Resuable "block"s */

block::scalar_type::Ptr int_type = nullptr;
block::scalar_type::Ptr char_type = nullptr;
block::scalar_type::Ptr float_type = nullptr;
block::scalar_type::Ptr void_type = nullptr;
block::int_const::Ptr const_one = nullptr;
block::int_const::Ptr const_zero = nullptr;

/*--- */

DynamicByDefaultVisitor::DynamicByDefaultVisitor(
        clang::ASTContext * context, std::vector<block::block::Ptr> & new_decls)
    : context(context), new_decls(new_decls)
{
}

block::stmt_block::Ptr DynamicByDefaultVisitor::promote_to_body(clang::Stmt * s)
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

block::type::Ptr DynamicByDefaultVisitor::lower_type(clang::QualType type)
{
        auto t = type.getTypePtr();
        if (type->isVoidType())
                return void_type;
        else if (auto bit = dyn_cast<clang::BuiltinType>(t))
        {
                switch (bit->getKind())
                {
                        case clang::BuiltinType::Char_U:
                        case clang::BuiltinType::Char_S:
                                return char_type;
                        case clang::BuiltinType::UInt:
                        case clang::BuiltinType::Int:
                                return int_type;
                        case clang::BuiltinType::Float:
                                return float_type;
                        default:
                                bit->dump();
                                llvm_unreachable("Cannot handle builtin type");
                }

                llvm_unreachable("Why are we here? Just to suffer?");
                if (bit->isInteger())
                        return int_type;
                if (bit->isFloatingPoint())
                        return float_type;
        }
        if (const clang::PointerType * pt = dyn_cast<clang::PointerType>(t))
        {
                auto bpt = std::make_shared<block::pointer_type>();
                bpt->pointee_type = lower_type(pt->getPointeeType());
                return bpt;
        }
        if (const clang::ConstantArrayType * cat
            = dyn_cast<const clang::ConstantArrayType>(t))
        {
                auto bat = std::make_shared<block::array_type>();
                bat->element_type = lower_type(cat->getElementType());
                bat->size = cat->getSize().getSExtValue();
                return bat;
        }
        if (const clang::DecayedType * dt
            = dyn_cast<const clang::DecayedType>(t))
        {
                return lower_type(dt->getDecayedType());
        }
        type.dump();
        llvm_unreachable("Can't resolve type");
}

block::type::Ptr DynamicByDefaultVisitor::wrap_type(block::type::Ptr type)
{
        auto bt = std::make_shared<block::builder_var_type>();
        bt->closure_type = type;
        bt->builder_var_type_id = block::builder_var_type::DYN_VAR;
        return bt;
}

block::var::Ptr
DynamicByDefaultVisitor::lower_function_arg(clang::ParmVarDecl * p)
{
        block::var::Ptr v = std::make_shared<block::var>();
        v->var_name = p->getNameAsString();
        v->var_type = lower_type(p->getType());
        return v;
}

block::expr::Ptr DynamicByDefaultVisitor::create_compound_expr(
        clang::BinaryOperator * bo, block::binary_expr::Ptr bbe)
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

block::binary_expr::Ptr
maybe_compound_binary_opcode_to_ptr(clang::BinaryOperatorKind op)
{
        switch (op)
        {
                // Operations like MulAssign need to be
                // handled separately
                case clang::BO_MulAssign:
                        return std::make_shared<block::mul_expr>();
                case clang::BO_DivAssign:
                        return std::make_shared<block::div_expr>();
                case clang::BO_RemAssign:
                        return std::make_shared<block::mod_expr>();
                case clang::BO_AddAssign:
                        return std::make_shared<block::plus_expr>();
                case clang::BO_SubAssign:
                        return std::make_shared<block::minus_expr>();
                case clang::BO_ShlAssign:
                        return std::make_shared<block::lshift_expr>();
                case clang::BO_ShrAssign:
                        return std::make_shared<block::rshift_expr>();
                case clang::BO_AndAssign:
                        return std::make_shared<block::bitwise_and_expr>();
                case clang::BO_XorAssign:
                        return std::make_shared<block::bitwise_xor_expr>();
                case clang::BO_OrAssign:
                        return std::make_shared<block::bitwise_or_expr>();
                default:
                        return nullptr;
        }
}

block::binary_expr::Ptr
simple_binary_opcode_to_ptr(clang::BinaryOperatorKind op)
{
        switch (op)
        {
                case clang::BO_Add:
                        return std::make_shared<block::plus_expr>();
                case clang::BO_LT:
                        return std::make_shared<block::lt_expr>();
                case clang::BO_GT:
                        return std::make_shared<block::gt_expr>();
                case clang::BO_LE:
                        return std::make_shared<block::lte_expr>();
                case clang::BO_GE:
                        return std::make_shared<block::gte_expr>();
                case clang::BO_LAnd:
                        return std::make_shared<block::and_expr>();
                case clang::BO_LOr:
                        return std::make_shared<block::or_expr>();
                case clang::BO_NE:
                        return std::make_shared<block::ne_expr>();
                case clang::BO_EQ:
                        return std::make_shared<block::equals_expr>();
                case clang::BO_Rem:
                        return std::make_shared<block::mod_expr>();
                case clang::BO_Mul:
                        return std::make_shared<block::mul_expr>();
                case clang::BO_Div:
                        return std::make_shared<block::div_expr>();
                case clang::BO_Sub:
                        return std::make_shared<block::minus_expr>();
                case clang::BO_Shl:
                        return std::make_shared<block::lshift_expr>();
                case clang::BO_Shr:
                        return std::make_shared<block::rshift_expr>();
                case clang::BO_And:
                        return std::make_shared<block::bitwise_and_expr>();
                case clang::BO_Xor:
                        return std::make_shared<block::bitwise_xor_expr>();
                case clang::BO_Or:
                        return std::make_shared<block::bitwise_or_expr>();
                default:
                        return nullptr;
        }
}

block::expr::Ptr DynamicByDefaultVisitor::lower_expr(clang::Expr * e)
{
        if (e == nullptr)
                llvm_unreachable("Expr cannot be null?");

        if (clang::ParenExpr * pe = dyn_cast<clang::ParenExpr>(e))
                return lower_expr(pe->getSubExpr());

        if (clang::IntegerLiteral * il = dyn_cast<clang::IntegerLiteral>(e))
        {
                auto ic = std::make_shared<block::int_const>();
                ic->value = il->getValue().getSExtValue();
                ic->is_64bit = false;
                return ic;
        }

        if (clang::CharacterLiteral * cl = dyn_cast<clang::CharacterLiteral>(e))
        {
                auto ic = std::make_shared<block::int_const>();
                ic->value = cl->getValue();
                ic->is_64bit = false;
                return ic;
        }

        if (clang::BinaryOperator * bo = dyn_cast<clang::BinaryOperator>(e))
        {
                if (bo->getOpcode() == clang::BO_Assign)
                {
                        auto a = std::make_shared<block::assign_expr>();
                        a->var1 = lower_expr(bo->getLHS());
                        a->expr1 = lower_expr(bo->getRHS());
                        return a;
                }

                // Handle the regular binary operators
                block::binary_expr::Ptr maybe_assign_op
                        = maybe_compound_binary_opcode_to_ptr(bo->getOpcode());
                if (maybe_assign_op)
                        return create_compound_expr(bo, maybe_assign_op);

                block::binary_expr::Ptr be
                        = simple_binary_opcode_to_ptr(bo->getOpcode());
                if (!be)
                {
                        bo->dump();
                        llvm_unreachable("Cannot handle binary operator");
                }
                be->expr1 = lower_expr(bo->getLHS());
                be->expr2 = lower_expr(bo->getRHS());
                return be;
        }

        if (clang::UnaryOperator * uo = dyn_cast<clang::UnaryOperator>(e))
        {
                // TODO (rdubi): clean up unary op handling, add support for prefix ops
                // Most unary operators can be handled as unary
                // operators, some need special handling as binary
                // operators and expressions

                if (uo->getOpcode() == clang::UO_PostInc)
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
                else if (uo->getOpcode() == clang::UO_PostDec)
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
                else if (uo->getOpcode() == clang::UO_Deref)
                {
                        auto sq = std::make_shared<block::sq_bkt_expr>();
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
                                case clang::UO_AddrOf:
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
        else if (clang::CastExpr * ice = dyn_cast<clang::CastExpr>(e))
        {
                if (clang::ImplicitCastExpr * ice
                    = dyn_cast<clang::ImplicitCastExpr>(e))
                {
                        // All implicit casts can actually be lowered as
                        // it is, There is special cast with void*
                        // ofcourse due to how C and C++ treat them
                        // TODO: handle void* to T* casts
                        return lower_expr(ice->getSubExpr());
                }
                if (ice->getCastKind() == clang::CK_LValueToRValue)
                {
                        return lower_expr(ice->getSubExpr());
                }
                else
                {
                        ice->dump();
                        llvm_unreachable("Cannot handle cast operation");
                }
        }
        else if (clang::DeclRefExpr * dr = dyn_cast<clang::DeclRefExpr>(e))
        {
                clang::ValueDecl * vd = dr->getDecl();
                auto v = std::make_shared<block::var>();
                v->var_name = vd->getNameAsString();
                // type here doesn't matter, but cannot be left null
                v->var_type = int_type;
                auto ve = std::make_shared<block::var_expr>();
                ve->var1 = v;
                return ve;
        }
        if (clang::ArraySubscriptExpr * ase
            = dyn_cast<clang::ArraySubscriptExpr>(e))
        {
                auto sbe = std::make_shared<block::sq_bkt_expr>();
                sbe->var_expr = lower_expr(ase->getLHS());
                sbe->index = lower_expr(ase->getRHS());
                return sbe;
        }

        if (clang::CallExpr * ce = dyn_cast<clang::CallExpr>(e))
        {
                auto bce = std::make_shared<block::function_call_expr>();
                bce->expr1 = lower_expr(ce->getCallee());
                for (auto arg : ce->arguments())
                        bce->args.push_back(lower_expr(arg));
                return bce;
        }

        if (clang::InitListExpr * ile = dyn_cast<clang::InitListExpr>(e))
        {
                auto bile = std::make_shared<block::initializer_list_expr>();
                for (auto in : ile->inits())
                        bile->elems.push_back(lower_expr(in));
                return bile;
        }

        if (clang::StringLiteral * sl = dyn_cast<clang::StringLiteral>(e))
        {
                auto bse = std::make_shared<block::string_const>();
                bse->value = sl->getString().str();
                return bse;
        }
        e->dump();
        llvm_unreachable("Cannot handle expr type");
        return nullptr;
}

static inline bool should_be_dynamic(const std::string & name)
{
        return std::find(make_static.cbegin(), make_static.cend(), name)
                == make_static.cend();
}

static inline block::type::Ptr
make_dyn_as_needed(block::type::Ptr t, const std::string & name)
{
        block::builder_var_type::Ptr res
                = std::make_shared<block::builder_var_type>();

        res->closure_type = t;
        res->builder_var_type_id = should_be_dynamic(name)
                ? block::builder_var_type::DYN_VAR
                : block::builder_var_type::STATIC_VAR;

        return res;
}

void DynamicByDefaultVisitor::handle_decl_stmt(
        clang::DeclStmt * ds, std::vector<block::stmt::Ptr> & out)
{
        for (auto de : ds->decls())
        {
                clang::VarDecl * vd = dyn_cast<clang::VarDecl>(de);

                if (!vd)
                {
                        de->dump();
                        llvm_unreachable("Cannot handle decl in decl_stmt");
                        continue;
                }

                // TODO figure out if we need to reuse vars or is it
                // okay for them to be separate
                block::var::Ptr v = std::make_shared<block::var>();
                // TODO (rdubi): is this a global name?
                // NOTE (rdubi): doesn't seem that way
                v->var_name = vd->getName().str();
                printf("Var is called %s\n", v->var_name.c_str());


                // NOTE (rdubi): this seems to be the part that decides
                // "staticity"
                v->var_type = make_dyn_as_needed(
                        lower_type(vd->getType()), v->var_name);

                block::decl_stmt::Ptr d = std::make_shared<block::decl_stmt>();
                d->decl_var = v;
                d->init_expr
                        = vd->hasInit() ? lower_expr(vd->getInit()) : nullptr;
                out.push_back(d);
        }
}

void DynamicByDefaultVisitor::lower_stmt_into(
        clang::Stmt * s, std::vector<block::stmt::Ptr> & out)
{
        if (clang::CompoundStmt * cs = dyn_cast<clang::CompoundStmt>(s))
        {
                block::stmt_block::Ptr nb
                        = std::make_shared<block::stmt_block>();
                for (auto s : cs->body())
                        lower_stmt_into(s, nb->stmts);
                out.push_back(nb);
                return;
        }
        if (clang::DeclStmt * ds = dyn_cast<clang::DeclStmt>(s))
        {
                handle_decl_stmt(ds, out);
                return;
        }
        if (clang::ForStmt * fs = dyn_cast<clang::ForStmt>(s))
        {
                block::for_stmt::Ptr bfs = std::make_shared<block::for_stmt>();
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
        else if (clang::WhileStmt * ws = dyn_cast<clang::WhileStmt>(s))
        {
                auto bws = std::make_shared<block::while_stmt>();
                bws->cond = lower_expr(ws->getCond());
                bws->body = promote_to_body(ws->getBody());
                out.push_back(bws);
                return;
        }
        else if (clang::IfStmt * is = dyn_cast<clang::IfStmt>(s))
        {
                auto bis = std::make_shared<block::if_stmt>();
                bis->cond = lower_expr(is->getCond());
                if (is->getThen())
                        bis->then_stmt = promote_to_body(is->getThen());
                else
                        bis->then_stmt = std::make_shared<block::stmt_block>();
                if (is->getElse())
                        bis->else_stmt = promote_to_body(is->getElse());
                else
                        bis->else_stmt = std::make_shared<block::stmt_block>();
                out.push_back(bis);
                return;
        }
        else if (clang::ValueStmt * vs = dyn_cast<clang::ValueStmt>(s))
        {
                if (vs->getExprStmt() == nullptr)
                        return;
                auto es = std::make_shared<block::expr_stmt>();
                es->expr1 = lower_expr(vs->getExprStmt());
                out.push_back(es);
                return;
        }
        else if (clang::ReturnStmt * rs = dyn_cast<clang::ReturnStmt>(s))
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

bool DynamicByDefaultVisitor::VisitFunctionDecl(clang::FunctionDecl * fd)
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

DynamicByDefaultConsumer::DynamicByDefaultConsumer(clang::ASTContext * context)
    : Visitor(context, new_decls)
{
}

void DynamicByDefaultConsumer::HandleTranslationUnit(
        clang::ASTContext & context)
{
        Visitor.TraverseDecl(context.getTranslationUnitDecl());


        // Dump all the generated decls
        std::stringstream ss;
        for (auto decl : new_decls)
                block::c_code_generator::generate_code(decl, ss, 0);
        llvm::errs() << ss.str();
}

std::unique_ptr<clang::ASTConsumer> DynamicByDefaultAction::CreateASTConsumer(
        clang::CompilerInstance & CI, llvm::StringRef infile)
{
        return std::unique_ptr<clang::ASTConsumer>(
                new DynamicByDefaultConsumer(&CI.getASTContext()));
}

DynamicByDefaultFrontendActionFactory::DynamicByDefaultFrontendActionFactory()
{
}

std::unique_ptr<clang::FrontendAction>
DynamicByDefaultFrontendActionFactory::create()
{
        return std::make_unique<DynamicByDefaultAction>();
}

// TODO: factor out
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
                auto OptionsParser
                        = clang::tooling::CommonOptionsParser::create(
                                argc, argv, DynamicByDefaultCategory);
                if (!OptionsParser)
                        return -1;
                clang::tooling::ClangTool Tool(
                        OptionsParser->getCompilations(),
                        OptionsParser->getSourcePathList());
                DynamicByDefaultFrontendActionFactory Factory;
                printf("About to run\n");
                Tool.run(&Factory);
                return 0;
        }
        return -1;
}
