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
#include<stdio.h>
#include <stdlib.h>
#define empt NULL;
typedef void* node;
typedef node (*func)(node);
typedef node* list;

list range(long i, long f) {
   list l = realloc(NULL, (f-i+1) * sizeof(node *));
   long x;
   for (x=i; x<=f; x++)
      l[x] = x;
   return l;
}

node assign(node a, node b) {
   a=b;
   return empt;
}
main() {
   node $fibo = 0;
   node $res = 0;
   printf("Hello World");
}
