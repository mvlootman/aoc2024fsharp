// For more information see https://aka.ms/fsharp-console-apps

let debug<'T>  (label : string)  (value : 'T) =
     printfn $"[ {label} ]>>>: {value}"
     value

// let debug value =
    // debugWithLabel "Go" value

let l = [1;2;3;4]

let res = l
        |> debug "a"
        |> List.sum
        |> debug "bb"
        |> fun x -> x + 1

printfn $"{res}"