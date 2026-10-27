module Shop.Domain.Account

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
