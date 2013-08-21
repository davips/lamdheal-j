package funcjava;

public class Add extends Node {
    public Node f(final Node first) {
        return new Lambda() {
            public Node body(Node second) {
                return new Number((Double)first.content + (Double)second.content);
            }
        };
    }
    
}
