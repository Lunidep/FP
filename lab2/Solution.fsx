// 5. Разбиение списка на пары элементов (т.е. [1,2,3,4] -> [(1,2),(3,4)])

// Method 1: Library Function
let LibFunc l = 
    let (ans, _, _, _) =  
        (([], 1, l, 0), l) ||> 
            List.fold (
                fun (acc, i, t, prev) el ->
                    let nt = List.tail t
                    if i % 2 > 0 then 
                        acc, i + 1, nt, el
                    else
                        (prev, el) :: acc, i + 1, nt, el
        )
    List.rev ans

// Method 2: Recursion
let Recursion l =
    let rec Next acc = function
        | [] | [_] -> List.rev acc
        | h1 :: h2 :: t ->
            Next ((h1, h2) :: acc) t   
    Next [] l

// Method 3: Tail Rec
let TailRec l =
    let rec Next = function
        | [] | [_] -> []
        | h1 :: h2 :: t -> 
            (h1, h2) :: (Next t)
    Next l

let main =
    let l = [1; 2; 3; 4]
    printfn "%A" (LibFunc l)
    printfn "%A" (Recursion l)
    printfn "%A" (TailRec l)

main
