type OrderLine = { Price: decimal; Quantity: int }

let discountThreshold = 1000m // грн
let discountRate = 0.10m
let vatRate = 0.20m // ПДВ

let lineTotal line = line.Price * decimal line.Quantity

let discountFor subtotal =
    if subtotal > discountThreshold then discountRate else 0m

/// Сума замовлення зі знижкою та ПДВ.
let orderTotal (lines: OrderLine list) =
    let subtotal = lines |> List.sumBy lineTotal
    subtotal * (1m - discountFor subtotal) * (1m + vatRate)
