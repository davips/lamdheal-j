package lamdheal;

import org.codehaus.commons.compiler.*;

public final class JavaCompiler {

    public static void
    main(String[] args) throws Exception {
        IScriptEvaluator se = CompilerFactoryFactory.getDefaultCompilerFactory().newScriptEvaluator();
        se.setReturnType(Double.class);
        se.setDefaultImports(null);
        se.setParameters(new String[]{"a", "b"}, new Class[]{Double.class, Double.class});
        se.setThrownExceptions(new Class[]{Exception.class});
        se.cook("System.out.print(a*b); return a/b;");
        Object res = se.evaluate(new Double[]{3d, 4d});
        System.out.println("Result = " + res);
    }

    private JavaCompiler() {
    }
}