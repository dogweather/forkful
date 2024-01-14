---
title:    "Rust: Розрахунок дати у майбутньому або минулому"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Чому

Розрахунок дати в майбутньому або минулому - це важлива задача для багатьох програм і проектів. Незалежно від того, чи програма потребує введення конкретної дати чи просто дати відправлення повідомлення, вміння обробляти цю задачу є необхідним для розробки ефективної програми. У цій статті ми розглянемо, як реалізувати це в Rust.

## Як

Для початку, нам потрібно імпортувати необхідні для роботи з датами бібліотеки. Однією із них є `chrono`, яка надає можливість простого та зворотного перетворення між різними форматами дати. Отже, попередньо ми напишемо:

```Rust
extern crate chrono;
use chrono::{DateTime, TimeZone, Utc};
```

Тепер, задля зручності, створимо функцію `calculate_date`, яка буде приймати на вхід кількість днів (позитивне для майбутньої дати, від'ємне - для минулої дати) та повертати обчислену дату. Нижче наведено код цієї функції:

```Rust
fn calculate_date(days: i64) -> DateTime<Utc> {
    let current_date = Utc::now();
    let calculated_date = current_date.checked_add_signed(chrono::Duration::days(days));
    return calculated_date.unwrap(); // перетворюємо нашу дату в UTC
}
```

## Глибоке дослідження

Як бачимо, все досить просто - ми використовуємо функцію `checked_add_signed` бібліотеки `chrono`, яка дозволяє додавати або віднімати певну кількість днів з поточної дати. Але що, якщо нам необхідно враховувати не тільки дні, але і години, хвилини та секунди? Тоді ми можемо використовувати функцію `checked_add` та передавати в неї створену спеціально для цього структуру `Duration`:

```Rust
fn calculate_date(days: i64, hours: i64, minutes: i64, seconds: i64) -> DateTime<Utc> {
    let current_date = Utc::now();
    let duration = chrono::Duration::days(days) + chrono::Duration::hours(hours) + chrono::Duration::minutes(minutes) + chrono::Duration::seconds(seconds);
    let calculated_date = current_date.checked_add_signed(duration);
    return calculated_date.unwrap();
}
```

Також важливо зазначити, що бібліотека `chrono` має багато інших корисних функцій та можливостей, які допоможуть вам у роботі з датами. Рекомендуємо ознайомитися з офіційною документацією для детальнішого дослідження.

## Дивіться також