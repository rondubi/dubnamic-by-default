#pragma once

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "blocks/block.h"
#include "blocks/c_code_generator.h"
#include "blocks/expr.h"
#include "blocks/stmt.h"
#include "blocks/var.h"

#include <memory>
#include <sstream>
#include <vector>

static llvm::cl::OptionCategory
        DynamicByDefaultCategory("dynamic-by-default options");

class DynamicByDefaultVisitor
    : public clang::RecursiveASTVisitor<DynamicByDefaultVisitor>
{
private:
        clang::ASTContext * context;
        std::vector<block::block::Ptr> & new_decls;

        block::stmt_block::Ptr promote_to_body(clang::Stmt * s);
        block::type::Ptr lower_type(clang::QualType type);
        block::type::Ptr wrap_type(block::type::Ptr type);
        block::var::Ptr lower_function_arg(clang::ParmVarDecl * p);
        block::expr::Ptr create_compound_expr(
                clang::BinaryOperator * bo, block::binary_expr::Ptr bbe);
        block::expr::Ptr lower_expr(clang::Expr * e);
        void
        lower_stmt_into(clang::Stmt * s, std::vector<block::stmt::Ptr> & out);

        // dubi's helpers
        void handle_decl_stmt(
                clang::DeclStmt * ds, std::vector<block::stmt::Ptr> & out);

        block::expr::Ptr
        handle_cxx_construct_expr(clang::CXXConstructExpr * cxce);

public:
        DynamicByDefaultVisitor(
                clang::ASTContext * context,
                std::vector<block::block::Ptr> & new_decls);
        bool VisitFunctionDecl(clang::FunctionDecl * fd);
};

class DynamicByDefaultConsumer : public clang::ASTConsumer
{
private:
        DynamicByDefaultVisitor Visitor;
        std::vector<block::block::Ptr> new_decls;

public:
        explicit DynamicByDefaultConsumer(clang::ASTContext * context);
        virtual void
        HandleTranslationUnit(clang::ASTContext & context) override;
};

class DynamicByDefaultAction : public clang::ASTFrontendAction
{
public:
        virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
                clang::CompilerInstance & CI, llvm::StringRef infile) override;
};

class DynamicByDefaultFrontendActionFactory
    : public clang::tooling::FrontendActionFactory
{
public:
        DynamicByDefaultFrontendActionFactory();
        virtual std::unique_ptr<clang::FrontendAction> create() override;
};
