package funcjava;

/**
 *
 * @author davi
 */
public abstract class Lambda extends Node {

    public abstract Node body(Node arg);
//    public abstract double body(double arg);

    public Node f(Node node) {
        return body(node);
    }
//    public double f(Node node) {
//        return body(node);
//    }
}
