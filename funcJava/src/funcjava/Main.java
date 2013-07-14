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
//        Long ti = System.currentTimeMillis();
//        final double[] $fibo = {0};
//        final double[] $res = {0};
//        final Node range = Runtime.range(1, 21100);
//        Runtime.apply(range, new Lambda() {
//            public Node body(Node arg) {
//                final double[] $n = {1};
//                final double[] $n_1 = {0};
//                final double[] $c = {1};
//
//                Runtime.apply(range, new Lambda() {
//                    public Node body(Node arg) {
//                        double temp = (new Add()).f($n).f($n_1); //), $c);
//                        $fibo.content = Runtime.apply(Runtime.apply(new Div(), temp), new Number(123456.0)).content;
//                        $c.content = Runtime.apply(Runtime.apply(new Add(), $c), new Number(1)).content;
//                        $n_1.content = $n.content;
//                        $n.content = $fibo.content;
//                        return new Empty();
//                    }
//                });
//
//                Runtime.reassign($res, Runtime.apply(Runtime.apply(new Add(), $res), $fibo));
//                return new Empty();
//            }
//        });
//        Runtime.apply(new Println(), $res);
//        System.out.println(10 * (System.currentTimeMillis() - ti) / 10000.0);

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
                        $fibo.content = (((Double) $n.content + (Double) $n_1.content + (Double) $c.content) / 123456.0);
                        $c.content=(Double)$c.content+1;
//                        Runtime.reassign($fibo, Runtime.apply(Runtime.apply(new Div(),    Runtime.apply(Runtime.apply(new Add(), Runtime.apply(Runtime.apply(new Add(), $n), $n_1)), $c)        ), new Number(123456.0)));
//                        Runtime.reassign($c, Runtime.apply(Runtime.apply(new Add(), $c), new Number(1)));
                        Runtime.reassign($n_1, $n);
                        Runtime.reassign($n, $fibo);
                        return null;
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
