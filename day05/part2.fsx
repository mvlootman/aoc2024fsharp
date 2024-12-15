let input =
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt""" |> _.Trim()

let example =
    """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""
        .Trim()

let parse (input: string) =
    let [| ordering; updates |] = input.Split("\n\n")

    let rules =
        ordering.Split("\n")
        |> Array.map _.Split("|")
        |> Array.map (fun [| before; after |] -> int before, int after)
        |> List.ofArray

    let updates =
        updates.Split("\n")
        |> Array.map (fun s -> s.Split(",") |> Array.map int |> List.ofArray)
        |> List.ofArray

    rules, updates

let rules, updates = parse input

let isValid a b =
    List.contains (b,a) rules |> not

let check numbers  =
    List.pairwise numbers
    |> List.map (fun (a,b) -> isValid a b )
    |> List.exists (fun x -> x = false)
    |> not

let takeMiddleNum numbers =
    let middle = (List.length numbers) /2
    numbers[middle]

let sortPages (a: int) (b: int) : int =
     match List.exists (fun pair -> pair =  (b,a)) rules with
     | true -> -1
     | _ -> 1

let answer =
    updates
    |> List.filter (fun x -> not (check x))
    |> List.map (List.sortWith sortPages)
    |> List.map takeMiddleNum
    |> List.sum

printfn $"day5 part2: {answer}"