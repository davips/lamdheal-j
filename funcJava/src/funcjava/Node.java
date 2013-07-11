package funcjava;

/**
 * Root of all entities allowable in a program.
 * @author davi
 */
public abstract class Node {
    /**
     * Implementing classes should describe here its behavior when applied.
     * @param n
     * @return
     */
    public abstract Node f(Node n);
    public Object content;
}
