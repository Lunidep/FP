// Попов Илья M80-306Б-20
// Вариант 16
// Функция f = (1 + 2*x^2) * exp(x^2)

open System
let f x = (1. + 2. * (x ** 2.)) * Math.Exp (x ** 2.)
let a = 0.0
let b = 1.0
let n = 10

let eps = 1e-4

let rec simple_iter f i a b = 
    if a <= b then simple_iter f (f a i) (a + 1) b
    else i
    
let sum = simple_iter (+) 0
let fact = simple_iter (*) 1 1
let power x = simple_iter (fun i acc -> x*acc) 1. 1

let rec iter start f acc prev =
  let new_acc: float = f start  acc prev
  let new_item = new_acc - acc
  if (abs new_item) <= eps then 
    (start , new_acc)
  else 
    iter (start  + 1) f new_acc new_item

//NATIVE Taylor
let native_next_item x n = (power x (2 * n)) * ((2. * (float n) + 1.) / float (fact n))

let taylor_naive x = 
  iter 0 (fun it acc prev -> acc + (native_next_item x it)) 0. 0.

//SMART Taylor
let smart_next_item x start  prev = prev * ((2. * (float start ) + 1.) / ((2. * ((float start ) - 1.) + 1.) * (float start ))) * (power x 2)

let taylor_smart x first_el = 
  let mutable prev = first_el
  let fn start acc prev= 
    let new_item = smart_next_item x start  prev 
    let new_acc = acc + new_item
    new_acc
  iter 1 fn first_el first_el


let main =
  printfn "|  x   |  func    |  naive   |  iters   |  smart   |  iters   |"
  printfn "---------------------------------------------------------------"
  for i=0 to n do
    let x = a+(float i)/(float n)*(b-a)
    let (native_iters, native_res) = taylor_naive x
    let (smart_iter, smart_res) = taylor_smart x (native_next_item x 0)
    printfn "|% 2.2f |% 2.6f |% 2.6f |    %2d    |% 2.6f |    %2d    |" x (f x) native_res native_iters smart_res smart_iter
  printfn "---------------------------------------------------------------"

main
