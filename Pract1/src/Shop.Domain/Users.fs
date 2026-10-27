module Shop.Domain.Users

/// Адреси електронної пошти активних користувачів.
let activeEmails (users: User list) =
    users
    |> List.filter (fun user -> user.IsActive)
    |> List.map (fun user -> user.Email.ToLowerInvariant())
