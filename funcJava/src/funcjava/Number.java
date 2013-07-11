package funcjava;

/**
 *
 * @author davi
 */
public class Number extends Node {

    public Number(double n) {
        this.content = n;
    }

    public Node f(Node n) {
        throw new UnsupportedOperationException("Numbers cannot be applied.");
    }

    @Override
    public String toString() {
        return ((Double) content).toString();
    }
}
