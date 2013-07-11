package funcjava;

/**
 *
 * @author davi
 */
class Println extends  Node {

    public Println() {
    }

    public Node f(Node n) {
        System.out.println(n);
        return new Empty();
    }
    
}
