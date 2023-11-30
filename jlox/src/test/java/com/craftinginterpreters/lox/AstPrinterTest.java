package com.craftinginterpreters.lox;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class AstPrinterTest {
    @Test
    public void printExpr() {
        Expr expression = new Expr.Binary(
                new Expr.Unary(new Token(TokenType.MINUS, "-", null, 1), new Expr.Literal(123)),
                new Token(TokenType.STAR, "*", null, 1),
                new Expr.Grouping(new Expr.Literal(45.67))
        );
        assertEquals("(* (- 123) (group 45.67))", new AstPrinter().print(expression));
    }

    @Test
    public void printCall() {
        AstPrinter printer = new AstPrinter();

        Expr fooCall = new Expr.Call(
                new Expr.Variable(new Token(TokenType.IDENTIFIER, "foo", null, 1)),
                new Token(TokenType.RIGHT_PAREN, ")", null, 1),
                List.of(new Expr.Literal("hello"), new Expr.Literal("world"))
        );
        assertEquals("foo(\"hello\", \"world\")", printer.print(fooCall));

        Expr barCall = new Expr.Call(
                fooCall,
                new Token(TokenType.RIGHT_PAREN, ")", null, 1),
                List.of(new Expr.Literal("lox"))
        );
        assertEquals("foo(\"hello\", \"world\")(\"lox\")", printer.print(barCall));

        Expr lastCall = new Expr.Call(
                barCall,
                new Token(TokenType.RIGHT_PAREN, ")", null, 1),
                List.of()
        );
        assertEquals("foo(\"hello\", \"world\")(\"lox\")()", printer.print(lastCall));
    }
}
