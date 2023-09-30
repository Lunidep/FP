// Попов Илья M80-306Б-20
// Вариант 16 (функции 16, 17, 18)

open System

let eps = 1e-6

let rec iter f acc =
  let new_acc = f acc
  if (abs (acc - new_acc)) <= eps then 
    new_acc
  else 
    iter f new_acc

let  update_borders f (a : float) (b : float) = 
  let c = (a + b) / 2.
  if ((f b) * (f c)) < 0. then 
    (c, b)
  else
    (a, c)


let rec dichotomy f (a : float) (b : float) = 
  if (b - a) > eps then  
    let (new_a : float, new_b : float) =  update_borders f a b 
    dichotomy f new_a new_b
  else
    ((a + b) / 2.)


let iterations phi x0 = 
  let next acc = (phi acc)
  iter next x0


let newthon f f' x0 = 
  let next acc = acc - (f acc) / (f' acc)
  iter next x0
  

//Equation 16
let f1 x = 3. * sin(Math.Sqrt(x)) + 0.35 * x - 3.8
let f1' x = (3. * Math.Cos(Math.Sqrt(x)) / 2. * Math.Sqrt(x)) + 0.35
let phi1 x = (1. / 0.35) * (3.8 - 3. * Math.Sin(Math.Sqrt(x)))

//Equation 17
let f2 x = 0.25 * x ** 3. + x - 1.2502 
let f2' x = 0.25 * (x ** 3.) + 1. 
let phi2 x = 1.2502 - 0.25 * (x ** 3.)

//Equation 18
let f3 x = x + Math.Sqrt(x) + x ** (1. / 3.) - 2.5
let f3' x = 1. + (1. / (2. * Math.Sqrt(x))) + (1. / (3. * (x ** (2. / 3.))))
let phi3 x = 2.5 - Math.Sqrt(x) - (x ** (1. / 3.))

let main = 
    printfn "%5.7f  %5.7f  %5.7f" (dichotomy f1 2. 3.) (iterations phi1 2.5) (newthon f1 f1' 2.5)
    printfn "%5.7f  %5.7f  %5.7f" (dichotomy f2 0. 2.) (iterations phi2 1.) (newthon f2 f2' 1.)
    printfn "%5.7f  %5.7f  %5.7f" (dichotomy f3 0.4 1.) (iterations phi3 0.7) (newthon f3 f3' 0.7)
