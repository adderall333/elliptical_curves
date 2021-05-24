open System
open Library.EllipticalCurves

[<EntryPoint>]
let main argv =
    printfn "Set p"
    let p = Console.ReadLine() |> int
    
    printfn "Set a"
    let a = Console.ReadLine() |> int
    
    printfn "Set b"
    let b = Console.ReadLine() |> int
    Console.WriteLine()
    
    printfn $"{getResult p a b}"
    
    printfn "Press any key to exit"
    Console.ReadKey() |> ignore
    0