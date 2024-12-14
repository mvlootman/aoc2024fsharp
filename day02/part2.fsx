let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}/input.txt"""
    |> List.ofSeq

let example =
    """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse (lines: string list) =
    lines
    |> List.map (fun line -> line.Split(" ") |> Array.map int |> Array.toList)


let is_monotonic (report: int list) =
    let asc = List.sort report
    let desc = List.sortDescending report
    report = asc || report = desc

let is_in_range (report: int list) =
    List.pairwise report
    |> List.forall (fun (a, b) -> [ 1..3 ] |> List.contains (abs (a - b)))

let check report =
    let is_monotonic = is_monotonic report
    let is_in_range = is_in_range report
    is_monotonic && is_in_range

let report_variants (report: int list) =
    seq {
        // original report
        yield report
        // all variants (len n-1)
        let length = (report |> List.length) - 1
        for i in [ 0..length ] -> report |> List.removeAt i
    }

let reports = parse input

let is_safe report =
    report_variants report |> Seq.exists check

let safe_count = reports |> List.filter is_safe |> List.length

printfn $"day2 part2: {safe_count}"