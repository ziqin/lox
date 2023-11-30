package com.craftinginterpreters.lox;

import java.util.List;

public class Interpreter implements Expr.Visitor<Object>, Stmt.Visitor<Void> {
    final Environment globals = new Environment();
    private Environment environment = globals;

    public Interpreter() {
        globals.define("clock", new LoxCallable() {
            @Override
            public int arity() {
                return 0;
            }

            @Override
            public Object call(Interpreter interpreter, List<Object> arguments) {
                return (double)System.currentTimeMillis() / 1000.0;
            }

            @Override
            public String toString() {
                return "<native fn>";
            }
        });
    }

    public void interpret(List<Stmt> statements) {
        try {
            for (Stmt statement : statements) {
                execute(statement);
            }
        } catch (RuntimeError error) {
            Lox.runtimeError(error);
        }
    }

    @Override
    public Object visitLiteralExpr(Expr.Literal expr) {
        return expr.value;
    }

    @Override
    public Object visitLogicalExpr(Expr.Logical expr) {
        Object left = evaluate(expr.left);
        // We evaluate the left operand first. We look at its value to see if we can short-circuit.
        // If not, and only then, do we evaluate the right operand.
        if (expr.operator.type() == TokenType.OR) {
            // Since Lox is dynamically typed, we allow operands of any type and use truthiness to
            // determine what each operand represents. We apply similar reasoning to the result.
            // Instead of promising to literally return true or false, a logic operator merely
            // guarantees it will return a value with appropriate truthiness.
            //
            // For example:
            //     print "hi" or 2; // "hi"
            //     print nil or "yes"; // "yes
            if (isTruthy(left)) return left;
        } else {
            if (!isTruthy(left)) return left;
        }
        return evaluate(expr.right);
    }

    @Override
    public Object visitGroupingExpr(Expr.Grouping expr) {
        return evaluate(expr.expression);
    }

    @Override
    public Object visitUnaryExpr(Expr.Unary expr) {
        Object right = evaluate(expr.right);
        return switch (expr.operator.type()) {
            case BANG -> !isTruthy(right);
            case MINUS -> {
                checkNumberOperand(expr.operator, right);
                yield -(double)right;
            }
            // Unreachable
            default -> null;
        };
    }

    @Override
    public Object visitVariableExpr(Expr.Variable expr) {
        return environment.get(expr.name);
    }

    @Override
    public Object visitBinaryExpr(Expr.Binary expr) {
        // We evaluate the operands in left-to-right order.
        // If those operands have side effects, this choice is user visible.
        Object left = evaluate(expr.left);
        Object right = evaluate(expr.right);
        return switch (expr.operator.type()) {
            case BANG_EQUAL -> !isEqual(left, right);
            case EQUAL_EQUAL -> isEqual(left, right);
            case GREATER -> {
                checkNumberOperands(expr.operator, left, right);
                yield (double)left > (double)right;
            }
            case GREATER_EQUAL -> {
                checkNumberOperands(expr.operator, left, right);
                yield (double)left >= (double)right;
            }
            case LESS -> {
                checkNumberOperands(expr.operator, left, right);
                yield (double)left < (double)right;
            }
            case LESS_EQUAL -> {
                checkNumberOperands(expr.operator, left, right);
                yield (double)left <= (double)right;
            }
            case MINUS -> {
                checkNumberOperands(expr.operator, left, right);
                yield (double)left - (double)right;
            }
            case SLASH -> {
                checkNumberOperands(expr.operator, left, right);
                yield (double)left / (double)right;
            }
            case STAR -> {
                checkNumberOperands(expr.operator, left, right);
                yield (double)left * (double)right;
            }
            case PLUS -> {
                if (left instanceof Double l && right instanceof Double r) {
                    yield l + r;
                }
                if (left instanceof String l && right instanceof String r) {
                    yield l + r;
                }
                throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.");
            }
            // Unreachable.
            default -> null;
        };
    }

    @Override
    public Object visitCallExpr(Expr.Call expr) {
        Object callee = evaluate(expr.callee);

        // Since argument expressions may have side effects,
        // the order they are evaluated could be user visible.
        List<Object> arguments = expr.arguments.stream()
                .map(this::evaluate)
                .toList();

        if (!(callee instanceof LoxCallable function)) {
            throw new RuntimeError(expr.paren, "Can only call functions and classes.");
        }
        if (arguments.size() != function.arity()) {
            throw new RuntimeError(expr.paren,
                    String.format("Expected %d arguments but got %d.", function.arity(), arguments.size()));
        }
        return function.call(this, arguments);
    }

    private Object evaluate(Expr expr) {
        return expr.accept(this);
    }

    private void execute(Stmt stmt) {
        stmt.accept(this);
    }

    void executeBlock(List<Stmt> statements, Environment environment) {
        Environment previous = this.environment;
        try {
            this.environment = environment;
            for (Stmt statement : statements) {
                execute(statement);
            }
        } finally {
            this.environment = previous;
        }
    }

    @Override
    public Void visitBlockStmt(Stmt.Block stmt) {
        executeBlock(stmt.statements, new Environment(environment));
        return null;
    }

    @Override
    public Void visitExpressionStmt(Stmt.Expression stmt) {
        evaluate(stmt.expression);
        return null;
    }

    // We take a function syntax node--a compile-time representation of the function--and convert it
    // to its runtime representation.
    @Override
    public Void visitFunctionStmt(Stmt.Function stmt) {
        LoxFunction function = new LoxFunction(stmt, environment);
        environment.define(stmt.name.lexeme(), function);
        return null;
    }

    @Override
    public Void visitIfStmt(Stmt.If stmt) {
        if (isTruthy(evaluate(stmt.condition))) {
            execute(stmt.thenBranch);
        } else if (stmt.elseBranch != null) {
            execute(stmt.elseBranch);
        }
        return null;
    }

    @Override
    public Void visitPrintStmt(Stmt.Print stmt) {
        Object value = evaluate(stmt.expression);
        System.out.println(stringify(value));
        return null;
    }

    @Override
    public Void visitReturnStmt(Stmt.Return stmt) {
        Object value = null;
        if (stmt.value != null) value = evaluate(stmt.value);

        // Since our own syntax tree evaluation is so heavily tied to the Java call stack, we're
        // pressed to do some heavyweight call stack manipulation occasionally, and exceptions are
        // a handy tool for that.
        throw new Return(value);
    }

    @Override
    public Void visitVarStmt(Stmt.Var stmt) {
        Object value = null;
        if (stmt.initializer != null) {
            value = evaluate(stmt.initializer);
        }
        environment.define(stmt.name.lexeme(), value);
        return null;
    }

    @Override
    public Void visitWhileStmt(Stmt.While stmt) {
        while (isTruthy(evaluate(stmt.condition))) {
            execute(stmt.body);
        }
        return null;
    }

    @Override
    public Object visitAssignExpr(Expr.Assign expr) {
        Object value = evaluate(expr.value);
        environment.assign(expr.name, value);
        // Return the assigned value because assignment is an expression that can be nested inside
        // other expressions.
        return value;
    }

    private static void checkNumberOperand(Token operator, Object operand) {
        if (operand instanceof Double) return;
        throw new RuntimeError(operator, "Operand must be a number.");
    }

    private static void checkNumberOperands(Token operator, Object left, Object right) {
        if (left instanceof Double && right instanceof Double) return;
        throw new RuntimeError(operator, "Operands must be numbers.");
    }

    private static boolean isTruthy(Object object) {
        if (object == null) return false;
        if (object instanceof Boolean o) return o;
        return false;
    }

    private static boolean isEqual(Object a, Object b) {
        if (a == null && b == null) return true;
        if (a == null) return false;
        // According to IEEE 754, NaN is not equal to itself.
        // In Java, the == operator on primitive doubles preserves that behavior, but the equals()
        // method on the Double class does not. Lox uses the latter, so doesn't follow IEEE.
        return a.equals(b);
    }

    private String stringify(Object object) {
        if (object == null) return "nil";

        if (object instanceof Double) {
            String text = object.toString();
            if (text.endsWith(".0")) {
                text = text.substring(0, text.length() - 2);
            }
            return text;
        }

        return object.toString();
    }
}
