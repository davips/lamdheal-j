package funcjava;

/**
 * Class just to test the performance of a functional-oriented Java.
 *
 * @author davi
 */
public class Main implements Runnable {

    public static void main(String[] args) {
//$fibo = 0
//$res=0
//l = [1 .. 1000]
//l (\i
//   $n=1
//   $n_1=0
//   (\j
//      $fibo = (i + j + $n + $n_1) / 123.456
//      $n_1 = $n
//      $n = $fibo
//   )
//   $res = $res + $fibo
//)
//
//` ("pseudofibo Lamdheal = " ++ (<< $res))

        Main M = new Main();
        M.run();
    }

    @Override
    public void run() {
        final Node $fibo = new Number(0);
        final Node $res = new Number(0);
        final Node range = Runtime.range(1, 21100);
        Runtime.apply(range, new Lambda() {
            public Node body(Node arg) {
                final Node $n = new Number(1);
                final Node $n_1 = new Number(0);
                final Node $c = new Number(1);

                Runtime.apply(range, new Lambda() {
                    public Node body(Node arg) {
                        Node temp = Runtime.apply(Runtime.apply(new Add(), Runtime.apply(Runtime.apply(new Add(), $n), $n_1)), $c);
//                        Runtime.reassign($fibo, Runtime.apply(Runtime.apply(new Div(), temp), new Number(123456.0)));
//                        Runtime.reassign($c, Runtime.apply(Runtime.apply(new Add(), $c), new Number(1)));
//                        Runtime.reassign($n_1, $n);
//                        Runtime.reassign($n, $fibo);
                        $fibo.content = Runtime.apply(Runtime.apply(new Div(), temp), new Number(123456.0)).content;
                        $c.content = Runtime.apply(Runtime.apply(new Add(), $c), new Number(1)).content;
                        $n_1.content = $n.content;
                        $n.content = $fibo.content;
                        return new Empty();
                    }
                });

                Runtime.reassign($res, Runtime.apply(Runtime.apply(new Add(), $res), $fibo));
                return new Empty();
            }
        });
        Runtime.apply(new Println(), $res);
    }
}
