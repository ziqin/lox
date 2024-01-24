package com.craftinginterpreters.lox;

public class AstPrinter implements Expr.Visitor<String> {
    public String print(Expr expr) {
        return expr.accept(this);
    }

    @Override
    public String visitBinaryExpr(Expr.Binary expr) {
        return parenthesize(expr.operator.lexeme(), expr.left, expr.right);
    }

    @Override
    public String visitCallExpr(Expr.Call expr) {
        StringBuilder builder = new StringBuilder(expr.callee.accept(this));
        builder.append('(');
        for (int i = 0; i < expr.arguments.size(); ++i) {
            if (i > 0) builder.append(", ");
            Expr argument = expr.arguments.get(i);
            builder.append(argument.accept(this));
        }
        builder.append(')');
        return builder.toString();
    }

    @Override
    public String visitGetExpr(Expr.Get expr) {
        return parenthesize(
                "get",
                expr.object.accept(this),
                expr.name.lexeme()
        );
    }

    @Override
    public String visitSetExpr(Expr.Set expr) {
        return parenthesize(
                "set",
                expr.object.accept(this),
                expr.name.lexeme(),
                expr.value.accept(this)
        );
    }

    @Override
    public String visitSuperExpr(Expr.Super expr) {
        return parenthesize("super", expr.method.lexeme());
    }

    @Override
    public String visitThisExpr(Expr.This expr) {
        return "this";
    }

    @Override
    public String visitGroupingExpr(Expr.Grouping expr) {
        return parenthesize("group", expr.expression);
    }

    @Override
    public String visitLiteralExpr(Expr.Literal expr) {
        if (expr.value == null) return "nil";
        return expr.value.toString();
    }

    @Override
    public String visitAssignExpr(Expr.Assign expr) {
        return parenthesize("assign", expr.value);
    }

    @Override
    public String visitLogicalExpr(Expr.Logical expr) {
        return parenthesize(expr.operator.lexeme(), expr.left, expr.right);
    }

    @Override
    public String visitUnaryExpr(Expr.Unary expr) {
        return parenthesize(expr.operator.lexeme(), expr.right);
    }

    @Override
    public String visitVariableExpr(Expr.Variable expr) {
        return expr.name.lexeme();
    }

    private String parenthesize(String name, Expr... exprs) {
        StringBuilder builder = new StringBuilder();
        builder.append('(').append(name);
        for (Expr expr : exprs) {
            builder.append(' ');
            builder.append(expr.accept(this));
        }
        builder.append(')');
        return builder.toString();
    }

    private String parenthesize(String name, String... strings) {
        StringBuilder builder = new StringBuilder();
        builder.append('(').append(name);
        for (String string : strings) {
            builder.append(' ');
            builder.append(string);
        }
        builder.append(')');
        return builder.toString();
    }
}
