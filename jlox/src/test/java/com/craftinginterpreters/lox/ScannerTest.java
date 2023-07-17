package com.craftinginterpreters.lox;

import org.junit.jupiter.api.Test;

import java.util.Iterator;

import static com.craftinginterpreters.lox.TokenType.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class ScannerTest {
    private static void assertEOF(Iterator<Token> iterator) {
        assertEquals(EOF, iterator.next().type());
        assertFalse(iterator.hasNext());
    }

    @Test
    public void parseSimpleAssignmentStatement() {
        String source = "var language = \"lox\";";
        Scanner scanner = new Scanner(source);
        Iterator<Token> itr = scanner.scanTokens().iterator();

        Token variable = itr.next();
        assertEquals(VAR, variable.type());
        assertEquals("var", variable.lexeme());

        Token identifier = itr.next();
        assertEquals(IDENTIFIER, identifier.type());
        assertEquals("language", identifier.lexeme());

        Token equal = itr.next();
        assertEquals(EQUAL, equal.type());
        assertEquals("=", equal.lexeme());

        Token string = itr.next();
        assertEquals(STRING, string.type());
        assertEquals("\"lox\"", string.lexeme());
        assertEquals("lox", string.literal());

        Token semicolon = itr.next();
        assertEquals(SEMICOLON, semicolon.type());
        assertEquals(";", semicolon.lexeme());

        assertEOF(itr);
    }

    @Test
    public void locationIsCorrect() {
        String source = """
                var
                language =
                    "lox"
                ;
                """;
        Scanner scanner = new Scanner(source);
        Iterator<Token> itr = scanner.scanTokens().iterator();

        Token variable = itr.next();
        assertEquals(1, variable.line());

        Token identifier = itr.next();
        assertEquals(2, identifier.line());

        Token equal = itr.next();
        assertEquals(2, equal.line());

        Token string = itr.next();
        assertEquals(3, string.line());

        Token semicolon = itr.next();
        assertEquals(4, semicolon.line());

        Token eof = itr.next();
        assertEquals(EOF, eof.type());
        assertEquals(5, eof.line());
    }

    @Test
    public void parseNumber() {
        Scanner scanner = new Scanner("123 456.25");
        Iterator<Token> itr = scanner.scanTokens().iterator();

        Token integer = itr.next();
        assertEquals(NUMBER, integer.type());
        assertEquals("123", integer.lexeme());
        assertEquals(123.0, integer.literal());

        Token floating = itr.next();
        assertEquals(NUMBER, floating.type());
        assertEquals("456.25", floating.lexeme());
        assertEquals(456.25, floating.literal());

        assertEOF(itr);
    }

    @Test
    public void parseString() {
        Scanner scanner = new Scanner("\"lox\" \"Hello, world\"");
        Iterator<Token> itr = scanner.scanTokens().iterator();

        Token lox = itr.next();
        assertEquals(STRING, lox.type());
        assertEquals("\"lox\"", lox.lexeme());
        assertEquals("lox", lox.literal());

        Token helloWorld = itr.next();
        assertEquals(STRING, helloWorld.type());
        assertEquals("\"Hello, world\"", helloWorld.lexeme());
        assertEquals("Hello, world", helloWorld.literal());

        assertEOF(itr);
    }

    @Test
    public void parseSymbols() {
        Scanner scanner = new Scanner("!+-*/==!=<=<>=>{}()=,.;");
        Iterator<Token> itr = scanner.scanTokens().iterator();

        assertEquals(BANG, itr.next().type());
        assertEquals(PLUS, itr.next().type());
        assertEquals(MINUS, itr.next().type());
        assertEquals(STAR, itr.next().type());
        assertEquals(SLASH, itr.next().type());
        assertEquals(EQUAL_EQUAL, itr.next().type());
        assertEquals(BANG_EQUAL, itr.next().type());
        assertEquals(LESS_EQUAL, itr.next().type());
        assertEquals(LESS, itr.next().type());
        assertEquals(GREATER_EQUAL, itr.next().type());
        assertEquals(GREATER, itr.next().type());
        assertEquals(LEFT_BRACE, itr.next().type());
        assertEquals(RIGHT_BRACE, itr.next().type());
        assertEquals(LEFT_PAREN, itr.next().type());
        assertEquals(RIGHT_PAREN, itr.next().type());
        assertEquals(EQUAL, itr.next().type());
        assertEquals(COMMA, itr.next().type());
        assertEquals(DOT, itr.next().type());
        assertEquals(SEMICOLON, itr.next().type());

        assertEOF(itr);
    }

    @Test
    public void whitespaceIgnored() {
        String source = "a = \r\nb    * \t  c;";
        Scanner scanner = new Scanner(source);
        Iterator<Token> itr = scanner.scanTokens().iterator();

        assertEquals(IDENTIFIER, itr.next().type());
        assertEquals(EQUAL, itr.next().type());
        assertEquals(IDENTIFIER, itr.next().type());
        assertEquals(STAR, itr.next().type());
        assertEquals(IDENTIFIER, itr.next().type());
        assertEquals(SEMICOLON, itr.next().type());

        assertEOF(itr);
    }

    @Test
    public void commentIgnored() {
        String source = """
                // Comment line
                var a = // Inline comment
                        4 / 2;
                """;
        Scanner scanner = new Scanner(source);
        Iterator<Token> itr = scanner.scanTokens().iterator();

        assertEquals(VAR, itr.next().type());
        assertEquals(IDENTIFIER, itr.next().type());
        assertEquals(EQUAL, itr.next().type());
        assertEquals(NUMBER, itr.next().type());
        assertEquals(SLASH, itr.next().type());
        assertEquals(NUMBER, itr.next().type());
        assertEquals(SEMICOLON, itr.next().type());

        assertEOF(itr);
    }

    @Test
    public void parseKeywords() {
        String source = """
                and
                class
                else
                false
                for
                fun
                if
                nil
                or
                print
                return
                super
                this
                true
                var
                while
                
                nonKeyword
                """;
        Scanner scanner = new Scanner(source);
        Iterator<Token> itr = scanner.scanTokens().iterator();

        assertEquals(AND, itr.next().type());
        assertEquals(CLASS, itr.next().type());
        assertEquals(ELSE, itr.next().type());
        assertEquals(FALSE, itr.next().type());
        assertEquals(FOR, itr.next().type());
        assertEquals(FUN, itr.next().type());
        assertEquals(IF, itr.next().type());
        assertEquals(NIL, itr.next().type());
        assertEquals(OR, itr.next().type());
        assertEquals(PRINT, itr.next().type());
        assertEquals(RETURN, itr.next().type());
        assertEquals(SUPER, itr.next().type());
        assertEquals(THIS, itr.next().type());
        assertEquals(TRUE, itr.next().type());
        assertEquals(VAR, itr.next().type());
        assertEquals(WHILE, itr.next().type());

        assertEquals(IDENTIFIER, itr.next().type());
        assertEOF(itr);
    }
}
