let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}/input.txt"""
    |> List.ofSeq

let example =
    """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse (input: string list) =
    seq {
        for rowIdx, row in Seq.indexed input do
            for colIdx, col in Seq.indexed row do
                ((colIdx, rowIdx), col.ToString())
    }
    |> Map.ofSeq

let grid = parse input

let startPositions =
    grid
    |> Map.filter (fun _k v -> v = "A")
    |> Map.keys
    |> Seq.toList

let get_all_dirs = [ [ (-1, -1); (1, 1); (-1, 1); (1, -1) ] ]

let get_chars grid coords =
    let x =
        coords
        |> List.map (fun coord -> Map.tryFind coord grid |> Option.defaultValue '?')

    System.String(x |> List.toArray)

let get_letters location coords shape =
    let (loc_x, loc_y) = location

    shape
    |> List.map (fun coord ->
        let shape_x, shape_y = coord
        let grid_coord = (loc_x + shape_x, loc_y + shape_y)

        Map.tryFind grid_coord coords
        |> Option.defaultValue "?")

let fold_fn acc coord =
    let xmas_count =
        List.map (get_letters coord grid) get_all_dirs
        |> List.filter (fun word ->
            List.contains
                word
                [ [ "M"; "S"; "M"; "S" ]
                  [ "S"; "M"; "S"; "M" ]
                  [ "M"; "S"; "S"; "M" ]
                  [ "S"; "M"; "M"; "S" ] ])
        |> List.length

    acc + xmas_count

let answer = List.fold fold_fn 0 startPositions
printfn $"day4 part2: {answer}"
