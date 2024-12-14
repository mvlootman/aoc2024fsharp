let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}/input.txt"""
    |> List.ofSeq

input



let example =
    """3   4
4   3
2   5
1   3
3   9
3   3"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse (lines: string list) =
    lines
    |> List.map (fun line -> line.Split("  ") |> Array.map int |> Array.toList)
    |> List.transpose

// parse example
let left, right = parse input |> fun lists -> (lists[0], lists[1])

let counts = List.countBy id right |> Map.ofList

let sum =
    left
    |> List.fold
        (fun acc elem ->
            let count = Map.tryFind elem counts |> Option.defaultValue 0
            acc + (elem * count))
        0

printfn $"day1 part2: {sum}"