package funcjava;

import java.util.ArrayList;

/**
 *
 * @author davi
 */
public class Runtime {

    public static Node apply(Node f, Node n) {
        return f.f(n);
    }

    public static Node reassign(Node ident, Node value) {
        ident.content = value.content;
        return new Empty();
    }

    static Node range(int i, int f) {
        ArrayList al = new ArrayList();
        for (int x = i; x <= f; x++) {
            al.add(new Number(x));
        }
        return new List(al);
    }
}
