module Shop.Domain.Parsing

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
