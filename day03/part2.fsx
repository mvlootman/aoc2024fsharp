open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""

let example =
    """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""

type Token =
    | Mul of x: int * y: int
    | Do
    | Dont


let parse input =
    let pattern = "mul\((\d+),(\d+)\)|do\(\)|don\'t\(\)"

    Regex.Matches(input, pattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        match m.Groups[0].Value with
        | "don't()" -> Dont
        | "do()" -> Do
        | _ -> Mul(m.Groups[1].Value |> int, m.Groups[2].Value |> int))

// fold_fun folds sequence of tokens and handles do/don't state transitions
// sums product of mul statements when active
let fold_fun (curr, active) token =
    match token with
    | Do -> (curr, true)
    | Dont -> (curr, false)
    | Mul (x, y) ->
        if active then
            (curr + x * y, active)
        else
            (curr, active)

let answer = parse input |> Seq.fold fold_fun (0, true)

printfn $"day3 part2: {answer}"
