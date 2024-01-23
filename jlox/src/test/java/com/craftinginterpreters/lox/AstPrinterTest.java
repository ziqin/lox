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
                List.of(new Expr.Variable(new Token(TokenType.IDENTIFIER, "hello", null, 1)),
                        new Expr.Variable(new Token(TokenType.IDENTIFIER, "world", null, 1)))
        );
        assertEquals("foo(hello, world)", printer.print(fooCall));

        Expr barCall = new Expr.Call(
                fooCall,
                new Token(TokenType.RIGHT_PAREN, ")", null, 1),
                List.of(new Expr.Variable(new Token(TokenType.IDENTIFIER, "lox", null, 1)))
        );
        assertEquals("foo(hello, world)(lox)", printer.print(barCall));

        Expr lastCall = new Expr.Call(
                barCall,
                new Token(TokenType.RIGHT_PAREN, ")", null, 1),
                List.of()
        );
        assertEquals("foo(hello, world)(lox)()", printer.print(lastCall));
    }

    @Test
    public void printGetExpr() {
        Token foo = new Token(TokenType.IDENTIFIER, "foo", null, 1);
        Token bar = new Token(TokenType.IDENTIFIER, "bar", null, 1);
        Expr getExpr = new Expr.Get(new Expr.Variable(foo), bar);
        assertEquals("(get foo bar)", new AstPrinter().print(getExpr));
    }

    @Test
    public void printSetExpr() {
        Expr.Variable foo = new Expr.Variable(new Token(TokenType.IDENTIFIER, "foo", null, 1));
        Token bar = new Token(TokenType.IDENTIFIER, "bar", null, 1);
        Expr.Literal literal = new Expr.Literal(123);
        Expr setExpr = new Expr.Set(foo, bar, literal);
        assertEquals("(set foo bar 123)", new AstPrinter().print(setExpr));
    }
}
