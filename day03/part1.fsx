open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""

let example =
    """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""

let parse input =
    let pattern = "mul\((\d+),(\d+)\)"

    Regex.Matches(input, pattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> (m.Groups[1].Value |> int, m.Groups[2].Value |> int))
    |> Seq.sumBy (fun (a, b) -> a * b)

let product = parse input

printfn $"day3 part1: {product}"
