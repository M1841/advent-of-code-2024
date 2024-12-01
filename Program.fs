let solutions =
  [| fun () -> // Day 1
       async {
         let path = "data/input1.txt"
         let! lines = path |> System.IO.File.ReadAllLinesAsync |> Async.AwaitTask

         let (leftList, rightList) =
           (([||], [||]), lines)
           ||> Array.fold (fun (leftList, rightList) line ->
             let ids = ([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries) |> line.Split

             ((leftList, [| ids[0] |> int |]) ||> Array.append, (rightList, [| ids[1] |> int |]) ||> Array.append))

         let pairs = (leftList |> Array.sort, rightList |> Array.sort) ||> Array.zip

         let distance =
           (0, pairs)
           ||> Array.fold (fun acc (leftId, rightId) -> acc + (leftId - rightId |> abs))

         printfn "%d" distance
       } |]

[<EntryPoint>]
let Main args =
  match args |> Array.isEmpty, args |> Array.tryItem 0 |> Option.map int with
  | false, Some index when index >= 1 && index <= (solutions |> Array.length) ->
    solutions[index - 1]() |> Async.RunSynchronously
    0
  | false, Some index when index >= 1 && index <= 25 ->
    printfn "Oops, I haven't solved that problem yet"
    0
  | false, _ ->
    printfn "Invalid argument. Provide a number from 1 to 25"
    0
  | _ ->
    printfn "Provide a number from 1 to 25 as a command-line argument"
    0
