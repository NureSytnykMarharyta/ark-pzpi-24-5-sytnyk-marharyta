module Shop.Domain.Pricing

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
