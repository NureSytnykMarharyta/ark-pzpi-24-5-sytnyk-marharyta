namespace Shop.Domain

/// Рядок замовлення: ціна за одиницю та кількість.
type OrderLine = { Price: decimal; Quantity: int }

/// Користувач магазину.
type User =
    {
        Name: string
        Email: string
        IsActive: bool
    }
