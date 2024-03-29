package com.craftinginterpreters.lox;

import java.util.List;

public class LoxFunction implements LoxCallable {
    private final Stmt.Function declaration;
    private final Environment closure;
    private final boolean isInitializer;

    public LoxFunction(Stmt.Function declaration, Environment closure, boolean isInitializer) {
        this.isInitializer = isInitializer;
        this.closure = closure;
        this.declaration = declaration;
    }

    LoxFunction bind(LoxInstance instance) {
        Environment environment = new Environment(closure);
        environment.define("this", instance);
        return new LoxFunction(declaration, environment, isInitializer);
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
        // A function encapsulates its parameters --- no other code outside the function can see
        // them. This means each function gets its own environment where it stores those variables.
        Environment environment = new Environment(closure);
        // We assume the parameter and argument lists have the same length. This is safe because
        // visitCallExpr() checks the arity before calling call().
        for (int i = 0; i < declaration.params.size(); i++) {
            environment.define(declaration.params.get(i).lexeme(), arguments.get(i));
        }
        try {
            interpreter.executeBlock(declaration.body, environment);
        } catch (Return returnValue) {
            if (isInitializer) {
                return closure.getAt(0, "this");
            }
            return returnValue.value;
        }
        // If it never catches one of these exceptions, it means the function reached the end of
        // its body without hitting a return statement. In that case, a non-initializer function
        // implicitly returns nil.
        if (isInitializer) {
            return closure.getAt(0, "this");
        }
        return null;
    }

    @Override
    public int arity() {
        return declaration.params.size();
    }

    // This gives nicer output if a user decides to print a function value.
    @Override
    public String toString() {
        return "<fn " + declaration.name.lexeme() + ">";
    }
}
