// Part 2

// Load the data
#load "one.fsx"

// Напечатайте среднюю оценку за каждый предмет
let subj = 
  subjs |> 
  Seq.map fst
let SubjectName x =
        subjs |> 
        Seq.filter (fun (name, _) -> name = x) |>
        Seq.map (fun (_, name) -> name) |>
        Seq.head 
let AvgMark x =
    let sum = 
        marks |> 
        Seq.filter (fun (_, name, _) -> name = x) |>
        Seq.map (fun (_, _, mark) -> mark) |>
        Seq.sum
    let cnt = 
        marks |> 
        Seq.filter (fun (_, name, _) -> name = x) |>
        Seq.length
    (float sum) / (float cnt)

let f1 =
        printfn "| Subject | Avegage Mark |"
        subj |> 
        Seq.iter (fun x -> printfn "%s | %.4f" (SubjectName x) (AvgMark x))
        printfn "____________________________________\n"
f1

// Для каждой группы, напечатайте список студентов, заваливших сессию (хотя бы одна оценка = 2)
let groups = 
  studs |> 
  Seq.map snd |> 
  Seq.distinct |> 
  Seq.sort
let studsGroup x = 
    studs |> 
    Seq.filter(fun (_, group) -> group = x) |>
    Seq.map(fun (name, _) -> name)
let checkMarks x =
    marks |>
    Seq.filter(fun (name, _, _) -> name = x) |>
    Seq.map(fun (_, _, mark) -> mark) |>
    Seq.filter(fun mark -> mark = 2) |>
    Seq.isEmpty

let f2 =
    printfn "Students who failed the session:\n"
    groups |>
    Seq.iter(fun x ->
        printfn "Group %d" x
        (studsGroup x) |>
        Seq.iter(fun x ->
                if not (checkMarks x) then
                    printfn "%s" x
            )
    )
    printfn "____________________________________\n"

f2

// Для каждого студента, найдите среднюю оценку
let students = 
  studs |>
  Seq.map fst

let AvgMarkStud x =
    let sum = 
        marks |> 
        Seq.filter (fun (name, _, _) -> name = x) |>
        Seq.map (fun (_, _, mark) -> mark) |>
        Seq.sum
    let cnt = 
        subjs |> 
        Seq.length
    (float sum) / (float cnt)

let f3 =
        printfn "| Strudent | Avegage Mark |"
        students |>
        Seq.iter (fun g ->
        printfn "%s | %.4f" g (AvgMarkStud g))
        printfn "____________________________________\n"

f3
