open System
open System.Diagnostics

// СПІЛЬНІ ТИПИ ТА КОНСТАНТИ

/// Товар і кількість його продажів.
type Product = { Name: string; Sold: int }

/// Замовлення: сума (грн), вага (кг) і місто доставки.
type Order =
    {
        Total: decimal
        WeightKg: decimal
        City: string
    }

/// Стаж клієнта (роки), з якого діє знижка постійного клієнта.
let loyalYears = 3

/// Знижка постійного клієнта (5 %).
let loyaltyRate = 0.05m

/// Частка суми, яку можна оплатити бонусами (30 %).
let maxBonusShare = 0.30m

/// Місто, з якого магазин відправляє замовлення.
let homeCity = "Харків"

/// Вага (кг), з якої замовлення вважається важким.
let heavyOrderKg = 30m

/// Підвищений тариф доставки: ціна за кілограм і надбавка (грн).
let premiumRatePerKg = 18m
let premiumSurcharge = 120m

/// Звичайний тариф доставки: ціна за кілограм і базова плата (грн).
let regularRatePerKg = 10m
let regularFee = 50m

/// Сума замовлення (грн), з якої звичайна доставка безкоштовна.
let freeDeliveryFrom = 1000m

// 1 ДО РЕФАКТОРИНГУ — Substitute Algorithm

module SubstituteAlgorithmBefore =
    /// Повертає count найпопулярніших товарів.
    let topProducts (count: int) (products: Product list) =
        let arr = List.toArray products
        for i in 0 .. arr.Length - 2 do
            for j in 0 .. arr.Length - 2 - i do
                if arr[j].Sold < arr[j + 1].Sold then
                    let tmp = arr[j]
                    arr[j] <- arr[j + 1]
                    arr[j + 1] <- tmp
        let result = ResizeArray<Product>()
        let mutable i = 0
        while i < count && i < arr.Length do
            result.Add(arr[i])
            i <- i + 1
        List.ofSeq result

// 1 ПІСЛЯ РЕФАКТОРИНГУ — Substitute Algorithm

module SubstituteAlgorithmAfter =
    /// Повертає count найпопулярніших товарів.
    let topProducts (count: int) (products: Product list) =
        products
        |> List.sortByDescending (fun product -> product.Sold)
        |> List.truncate count

// 2 ДО РЕФАКТОРИНГУ — Remove Assignments to Parameters

module RemoveAssignmentsToParametersBefore =
    /// Сума до сплати зі знижкою постійного клієнта й бонусами.
    let amountDue (total: decimal) (bonuses: decimal) (years: int) =
        let mutable total = total
        if years >= loyalYears then
            total <- total * (1m - loyaltyRate)
        if bonuses > 0m then
            total <- total - min bonuses (total * maxBonusShare)
        total <- Math.Round(total, 2)
        total

// 2 ПІСЛЯ РЕФАКТОРИНГУ — Remove Assignments to Parameters

module RemoveAssignmentsToParametersAfter =
    /// Сума до сплати зі знижкою постійного клієнта й бонусами.
    let amountDue (total: decimal) (bonuses: decimal) (years: int) =
        let discounted =
            if years >= loyalYears then total * (1m - loyaltyRate)
            else total
        let bonusLimit = discounted * maxBonusShare
        let paidByBonuses =
            if bonuses > 0m then min bonuses bonusLimit else 0m
        Math.Round(discounted - paidByBonuses, 2)

// 3 ДО РЕФАКТОРИНГУ — Decompose Conditional

module DecomposeConditionalBefore =
    /// Вартість доставки замовлення на задану дату.
    let deliveryCost (date: DateTime) (order: Order) =
        if
            (date.Month = 12 && date.Day >= 20)
            || (date.Month = 1 && date.Day <= 7)
            || (order.WeightKg > heavyOrderKg && order.City <> homeCity)
        then
            order.WeightKg * premiumRatePerKg + premiumSurcharge
        elif order.Total >= freeDeliveryFrom then
            0m
        else
            order.WeightKg * regularRatePerKg + regularFee

// 3 ПІСЛЯ РЕФАКТОРИНГУ — Decompose Conditional

module DecomposeConditionalAfter =
    /// Святковий період: з 20 грудня по 7 січня.
    let isHolidaySeason (date: DateTime) =
        (date.Month = 12 && date.Day >= 20)
        || (date.Month = 1 && date.Day <= 7)

    /// Важке замовлення в інше місто.
    let isHeavyIntercity (order: Order) =
        order.WeightKg > heavyOrderKg && order.City <> homeCity

    let premiumCost (order: Order) =
        order.WeightKg * premiumRatePerKg + premiumSurcharge

    let regularCost (order: Order) =
        if order.Total >= freeDeliveryFrom then 0m
        else order.WeightKg * regularRatePerKg + regularFee

    /// Вартість доставки замовлення на задану дату.
    let deliveryCost (date: DateTime) (order: Order) =
        if isHolidaySeason date || isHeavyIntercity order then
            premiumCost order
        else
            regularCost order

// ПЕРЕВІРКА ЗБЕРЕЖЕННЯ ПОВЕДІНКИ

let random = Random(2026)
let randomCases = 10_000
let cities = [| homeCity; "Київ"; "Львів" |]

/// Сума з копійками в діапазоні від 0 до maxValue грн.
let randomMoney (maxValue: int) = decimal (random.Next(0, maxValue * 100)) / 100m

/// Порівнює дві реалізації на всіх наборах вхідних даних і друкує результат.
let check name (before: 'a -> 'b) (after: 'a -> 'b) (inputs: 'a list) =
    let failed = inputs |> List.filter (fun input -> before input <> after input)
    let passed = inputs.Length - failed.Length
    let status = if failed.IsEmpty then "OK" else "ПОМИЛКА"
    printfn "%-34s %6d / %d  %s" name passed inputs.Length status
    failed |> List.truncate 3 |> List.iter (printfn "    розбіжність: %A")
    failed.IsEmpty

// 1. Substitute Algorithm
let randomProducts () =
    List.init (random.Next(0, 60)) (fun i -> { Name = $"P{i}"; Sold = random.Next(0, 20) })

let productCases =
    [ (3, []); (0, randomProducts ()); (-1, randomProducts ())
      (5, [ { Name = "A"; Sold = 7 }; { Name = "B"; Sold = 7 }; { Name = "C"; Sold = 9 } ]) ]
    @ List.init randomCases (fun _ -> random.Next(-2, 70), randomProducts ())

// 2. Remove Assignments to Parameters
let paymentCases =
    [ (1000m, 0m, 3); (1000m, -50m, 5); (1000m, 10_000m, 2); (0m, 100m, 4); (999.99m, 299.99m, 3) ]
    @ List.init randomCases (fun _ -> randomMoney 5000, randomMoney 3000 - 100m, random.Next(0, 11))

// 3. Decompose Conditional
let randomOrder () =
    { Total = randomMoney 3000
      WeightKg = decimal (random.Next(0, 600)) / 10m
      City = cities[random.Next(cities.Length)] }

let boundaryDates =
    [ DateTime(2026, 12, 19); DateTime(2026, 12, 20); DateTime(2027, 1, 7); DateTime(2027, 1, 8) ]

let boundaryOrders =
    [ { Total = 1000m; WeightKg = 30m; City = "Київ" }
      { Total = 999.99m; WeightKg = 30.1m; City = "Київ" }
      { Total = 1500m; WeightKg = 45m; City = homeCity } ]

let deliveryCases =
    [ for date in boundaryDates do
          for order in boundaryOrders -> date, order ]
    @ List.init randomCases (fun _ -> DateTime(2026, 1, 1).AddDays(float (random.Next(0, 730))), randomOrder ())

printfn "Перевірка збереження поведінки (%d випадкових + граничні випадки)" randomCases
printfn ""

let results =
    [ check "Substitute Algorithm"
          (fun (count, products) -> SubstituteAlgorithmBefore.topProducts count products)
          (fun (count, products) -> SubstituteAlgorithmAfter.topProducts count products)
          productCases
      check "Remove Assignments to Parameters"
          (fun (total, bonuses, years) -> RemoveAssignmentsToParametersBefore.amountDue total bonuses years)
          (fun (total, bonuses, years) -> RemoveAssignmentsToParametersAfter.amountDue total bonuses years)
          paymentCases
      check "Decompose Conditional"
          (fun (date, order) -> DecomposeConditionalBefore.deliveryCost date order)
          (fun (date, order) -> DecomposeConditionalAfter.deliveryCost date order)
          deliveryCases ]

// Порівняння швидкодії старого та нового алгоритму на 5 000 товарів.
let manyProducts = List.init 5000 (fun i -> { Name = $"P{i}"; Sold = random.Next(0, 100_000) })

/// Медіанний час (мс) з 15 запусків після прогрівання.
let measure (topProducts: int -> Product list -> Product list) =
    topProducts 10 manyProducts |> ignore
    let times =
        List.init 15 (fun _ ->
            let watch = Stopwatch.StartNew()
            topProducts 10 manyProducts |> ignore
            watch.Elapsed.TotalMilliseconds)
    (List.sort times)[7]

printfn ""
printfn "topProducts, 5000 товарів: до %.1f мс, після %.1f мс"
    (measure SubstituteAlgorithmBefore.topProducts)
    (measure SubstituteAlgorithmAfter.topProducts)

if List.forall id results then
    printfn "Усі перевірки пройдено."
else
    printfn "Є розбіжності!"
    exit 1
