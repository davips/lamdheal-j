package funcjava;

/**
 * Class to test the performance of a functional-oriented Java.
 *
 * @author davi
 */
public class Main implements Runnable {

    public static void main(String[] args) {
        Main M = new Main();
        M.run();
    }

    @Override
    public void run() {
        Long ti = System.currentTimeMillis();
        final Node $fibo = new Number(0);
        final Node $res = new Number(0);
        final Node range = Runtime.range(1, 15000);
        Runtime.apply(range, new Lambda() {
            public Node body(Node arg) {
                final Node $n = new Number(1);
                final Node $n_1 = new Number(0);
                final Node $c = new Number(1);
                Runtime.apply(range, new Lambda() {
                    public Node body(Node arg) {
                        Runtime.reassign($fibo, Runtime.apply(Runtime.apply(new Div(), Runtime.apply(Runtime.apply(new Add(), Runtime.apply(Runtime.apply(new Add(), $n), $n_1)), $c)), new Number(123456.0)));
                        Runtime.reassign($c, Runtime.apply(Runtime.apply(new Add(), $c), new Number(1)));
                        Runtime.reassign($n_1, $n);
                        Runtime.reassign($n, $fibo);
                        return new Empty();
                    }
                });

                Runtime.reassign($res, Runtime.apply(Runtime.apply(new Add(), $res), $fibo));
                return new Empty();
            }
        });
        Runtime.apply(new Println(), $res);
        System.out.println(10 * (System.currentTimeMillis() - ti) / 10000.0);
    }
}
