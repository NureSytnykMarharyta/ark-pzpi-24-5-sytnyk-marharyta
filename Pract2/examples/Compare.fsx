// Перевіряє, що код до і після рефакторингу дає однаковий результат.
#load "Before.fsx"
#load "After.fsx"

open Before
open After

let random = System.Random(42)

let randomLine () =
    let price = decimal (random.Next(1, 90000)) / 100m
    (price, random.Next(1, 8))

let randomOrder () =
    [ for _ in 1 .. random.Next(0, 6) -> randomLine () ]

let edgeCases = [ [ (1000m, 1) ]; [ (500m, 2) ]; [] ]

let orders = [ for _ in 1..2000 -> randomOrder () ] @ edgeCases

let toLine (price, quantity) = { Price = price; Quantity = quantity }

let sameResult order = calc order = orderTotal (List.map toLine order)

let mismatches = orders |> List.filter (not << sameResult)

printfn "Перевірено замовлень: %d" orders.Length
printfn "Розбіжностей: %d" mismatches.Length
