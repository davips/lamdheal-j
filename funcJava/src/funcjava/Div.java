package funcjava;

/**
 *
 * @author davi
 */
public class Div extends Node {
    public Node f(final Node up) {
        return new Lambda() {
            public Node body(Node down) {
                return new Number(((Double)up.content) / ((Double)down.content));
            }
        };
    }
    
}
