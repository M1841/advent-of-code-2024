let solutions =
  [| (fun () -> // Day 1
       // Part One
       let path = ("data", "input_1.txt") |> System.IO.Path.Combine
       let lines = path |> System.IO.File.ReadAllLines

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

       // Part Two
       let score =
         leftList
         |> Array.map (fun leftId ->
           (rightList |> Array.filter (fun rightId -> leftId = rightId) |> Array.length)
           * leftId)
         |> Array.sum

       printfn "%d" score)
     (fun () -> // Day 2
       // Part One
       let path = ("data", "input_2.txt") |> System.IO.Path.Combine
       let lines = path |> System.IO.File.ReadAllLines

       let reports =
         lines
         |> Array.map (fun line ->
           ([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
           |> line.Split
           |> Array.map (fun level -> int level))

       let IsReportSafe report =
         report
         |> Array.pairwise
         |> Array.forall (fun (level1, level2) ->
           (level1 - level2) |> abs <= 3
           && (level1 - level2) |> abs >= 1
           && (report = (report |> Array.sort) || report = (report |> Array.sortDescending)))

       let safeReportCount =
         reports |> Array.filter (fun report -> report |> IsReportSafe) |> Array.length

       printfn "%d" safeReportCount

       // Part Two
       let safeReportCountWithTolerance =
         reports
         |> Array.filter (
           (fun report ->
             report
             |> Array.mapi (fun i _ ->
               report
               |> Array.mapi (fun j level -> if j = i then None else Some level)
               |> Array.choose id)
             |> Array.exists (fun report -> report |> IsReportSafe))
         )
         |> Array.length

       printfn "%d" safeReportCountWithTolerance) |]

[<EntryPoint>]
let Main args =
  let solution =
    match args |> Array.isEmpty, args |> Array.tryItem 0 |> Option.map int with
    | false, Some index when index >= 1 && index <= (solutions |> Array.length) -> solutions[index - 1]()
    | false, Some index when index >= 1 && index <= 25 -> printfn "Oops, I haven't solved that problem yet"
    | false, _ -> printfn "Invalid argument. Provide a number from 1 to 25"
    | _ -> printfn "Provide a number from 1 to 25 as a command-line argument"

  solution
  0
