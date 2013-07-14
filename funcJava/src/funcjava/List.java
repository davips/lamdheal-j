package funcjava;

import java.util.ArrayList;
import java.util.Iterator;

/**
 *
 * @author davi
 */
public class List extends  Node {

    public ArrayList al;

    public List(ArrayList al) {
        this.al = al;
    }

    //foreach
    public Node f(Node node) {
        Iterator it = al.iterator();
        while(it.hasNext()) {
            node.f(null);//(Node) it.next());
            it.next();
        }
        return null;
    }

    //map
    //    public Node f(Node node) {
//        ArrayList al2 = new ArrayList<Node>(al.size());
//        for (int x = 0; x < al.size(); x++) {
//            al2.add(node.f((Node) al.get(x)));
//        }
//        return new List(al2);
//    }
}
