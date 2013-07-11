package funcjava;

/**
 *
 * @author davi
 */
public class Add extends Node {
    public Node f(final Node first) {
        return new Lambda() {
            public Node body(Node second) {
//                System.out.println(" >>" + n + "      " + arg + " ........ " + new Number(((Number)n).n + ((Number)arg).n));
                return new Number((Double)first.content + (Double)second.content);
            }
        };
    }
    
}
