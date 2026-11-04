namespace rec Shop.Domain

// В.1 Приклад оформлення програмного коду

module Pricing =

    /// Поріг суми замовлення (грн), після якого діє знижка.
    let discountThreshold = 1000m

    /// Знижка для великих замовлень (10 %).
    let discountRate = 0.10m

    /// Ставка ПДВ (20 %).
    let vatRate = 0.20m

    /// <summary>Повертає ціну зі знижкою.</summary>
    /// <param name="percent">Знижка, % (від 0 до 100).</param>
    /// <param name="price">Початкова ціна, грн.</param>
    /// <returns>Ціна після знижки.</returns>
    /// <exception cref="System.ArgumentOutOfRangeException">
    /// Якщо знижка поза межами 0–100.</exception>
    /// <example><c>applyDiscount 10m 200m</c> = 180m</example>
    let applyDiscount (percent: decimal) (price: decimal) : decimal =
        if percent < 0m || percent > 100m then
            raise (System.ArgumentOutOfRangeException(nameof percent))

        price * (1m - percent / 100m)

    /// Вартість одного рядка замовлення.
    let lineTotal (line: OrderLine) = line.Price * decimal line.Quantity

    /// Ставка знижки для заданої суми замовлення.
    let discountFor subtotal =
        if subtotal > discountThreshold then discountRate else 0m

    /// Сума замовлення зі знижкою та ПДВ.
    let orderTotal (lines: OrderLine list) =
        let subtotal = lines |> List.sumBy lineTotal
        subtotal * (1m - discountFor subtotal) * (1m + vatRate)

// В.2 Приклад програмного коду до рефакторингу

module Before =

    // рахуємо
    let calc(l:list<decimal*int>)=
      let mutable t=0m
      for (p,q) in l do t<-t+p*decimal q
      if t>1000m then t*0.9m*1.2m else t*1.2m

// В.3 Приклад програмного коду після рефакторингу

module After =

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

// В.4.1 Типи даних предметної області

/// Рядок замовлення: ціна за одиницю та кількість.
type OrderLine = { Price: decimal; Quantity: int }

/// Користувач магазину.
type User =
    {
        Name: string
        Email: string
        IsActive: bool
    }

// В.4.2 Константи, одиниці виміру та розмічене об’єднання

module Account =

    /// Максимальна кількість невдалих спроб входу.
    [<Literal>]
    let MaxLoginAttempts = 3

    /// Секунди.
    [<Measure>]
    type sec

    /// Час неактивності, після якого сесія завершується.
    let sessionTimeout = 30.0<sec>

    /// Стан облікового запису.
    type AccountStatus =
        | Active
        | Blocked

    /// Чи може користувач увійти після заданої кількості невдалих спроб.
    let canLogin (failedAttempts: int) (status: AccountStatus) =
        match status, failedAttempts with
        | Blocked, _ -> false
        | Active, MaxLoginAttempts -> false
        | Active, _ -> true

// В.4.3 Обробка помилок за допомогою типу Result

module Parsing =

    open System
    open System.Globalization

    /// Причини, з яких рядок не вдалося перетворити на ціну.
    type PriceError =
        | EmptyInput
        | NotANumber of string
        | Negative of decimal

    let private culture = CultureInfo.InvariantCulture

    /// Перетворює текст на ціну або повертає опис помилки.
    let parsePrice (text: string) : Result<decimal, PriceError> =
        if String.IsNullOrWhiteSpace text then
            Error EmptyInput
        else
            match Decimal.TryParse(text, NumberStyles.Number, culture) with
            | true, value when value < 0m -> Error(Negative value)
            | true, value -> Ok value
            | false, _ -> Error(NotANumber text)

// В.4.4 Конвеєр обробки колекції

module Users =

    /// Адреси електронної пошти активних користувачів.
    let activeEmails (users: User list) =
        users
        |> List.filter (fun user -> user.IsActive)
        |> List.map (fun user -> user.Email.ToLowerInvariant())

namespace Shop.Tests

// В.4.5 Модульні тести Expecto

module PricingTests =

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

namespace Shop.Examples

// В.4.6 Перевірка еквівалентності коду до і після рефакторингу

module Compare =

    // Перевіряє, що код до і після рефакторингу дає однаковий результат.
    open Shop.Domain.Before
    open Shop.Domain.After

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

// В.4.7 Файл проєкту з налаштуваннями перевірок компілятора

// <Project Sdk="Microsoft.NET.Sdk">
//
//   <PropertyGroup>
//     <TargetFramework>net8.0</TargetFramework>
//     <GenerateDocumentationFile>true</GenerateDocumentationFile>
//     <WarnOn>1182;3390</WarnOn>
//     <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
//   </PropertyGroup>
//
//   <ItemGroup>
//     <Compile Include="Types.fs" />
//     <Compile Include="Pricing.fs" />
//     <Compile Include="Account.fs" />
//     <Compile Include="Parsing.fs" />
//     <Compile Include="Users.fs" />
//   </ItemGroup>
//
// </Project>

// В.4.8 Налаштування форматування

// root = true
//
// [*]
// charset = utf-8
// end_of_line = lf
// insert_final_newline = true
//
// [*.{fs,fsx,fsi}]
// indent_size = 4
// max_line_length = 120
// fsharp_multiline_bracket_style = aligned

// В.4.9 Конвеєр безперервної інтеграції

// name: CI
//
// on: [push, pull_request]
//
// jobs:
//   build:
//     runs-on: ubuntu-latest
//     steps:
//       - uses: actions/checkout@v4
//       - uses: actions/setup-dotnet@v4
//         with:
//           dotnet-version: 8.0.x
//       - run: dotnet tool restore
//       - run: dotnet tool install --global dotnet-fsharplint
//       - run: dotnet fantomas --check .
//       - run: dotnet build -warnaserror
//       - run: dotnet fsharplint lint Shop.sln
//       - run: dotnet test
