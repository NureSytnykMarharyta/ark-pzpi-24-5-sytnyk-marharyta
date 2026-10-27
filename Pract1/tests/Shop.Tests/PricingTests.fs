module PricingTests

open Expecto
open Shop.Domain
open Shop.Domain.Pricing

[<Tests>]
let discountTests =
    testList "applyDiscount" [
        test "10% від 200 грн = 180 грн" {
            Expect.equal (applyDiscount 10m 200m) 180m "ціна"
        }

        test "знижка понад 100% — виняток" {
            Expect.throwsT<System.ArgumentOutOfRangeException>
                (fun () -> applyDiscount 150m 200m |> ignore)
                "некоректна знижка"
        }
    ]

[<Tests>]
let orderTotalTests =
    testList "orderTotal" [
        test "без знижки: 2 × 250 грн + ПДВ = 600 грн" {
            let order = [ { Price = 250m; Quantity = 2 } ]
            Expect.equal (orderTotal order) 600m "сума"
        }

        test "зі знижкою: 3 × 400 грн − 10 % + ПДВ = 1296 грн" {
            let order = [ { Price = 400m; Quantity = 3 } ]
            Expect.equal (orderTotal order) 1296m "сума"
        }
    ]
