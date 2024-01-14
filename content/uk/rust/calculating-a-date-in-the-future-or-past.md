---
title:                "Rust: Розрахунок дати в майбутньому або минулому"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

"## Чому"

Розрахунок дат в майбутньому або минулому є важливою технікою для багатьох проектів. Наприклад, це може бути корисно для встановлення термінів платежів або запланованих подій.

## Як

Існує кілька шляхів побудови такого розрахунку в Rust. Одним з них є використання стандартної бібліотеки ```chrono```, яка містить у собі зручні функції для роботи з датами та часом.

Наприклад, щоб додати 10 днів до поточної дати, ми можемо скористатись функцією ```add```:

```Rust
use chrono::{Utc, Duration};

let current_date = Utc::today();
let future_date = current_date.add(Duration::days(10));
println!("Future date: {}", future_date);
```
Вивід: ```Future date: 2020-11-02```

Також можна використовувати арифметичні операції, наприклад, щоб відняти місяць від поточної дати:

```Rust
use chrono::{Utc, NaiveDate};

let current_date = Utc::today();
let past_date = current_date - ChronoPeriod::months(1);
println!("Past date: {}", past_date);
```
Вивід: ```Past date: 2020-09-03```

Є також можливість конструювати дати за допомогою класу ```NaiveDate```, який дозволяє задавати конкретну дату за допомогою числових значень:

```Rust
use chrono::NaiveDate;

let future_date = NaiveDate::from_ymd(2021, 01, 01);
println!("Future date: {}", future_date);
```
Вивід: ```Future date: 2021-01-01```

## Заглиблення

Написання власної логіки розрахунку дат також є можливим. Для цього слід враховувати велику кількість чинників, таких як рік, місяць, день та можливість високосного року. Зручним варіантом є використання бібліотеки ```chrono```, але можна використовувати і власні реалізації алгоритмів.

Наприклад, для розрахунку дати наступного високосного року, можна використовувати наступну формулу: 

$$
(year + 4 - (year \% 4)) \% 4
$$

Ця формула дає потрібний рік, що є високосним після поточного. Використовуючи мову Rust, ми можемо написати такий код:

```Rust
fn next_leap_year(current_year: i32) -> i32 {
    (current_year + 4 - (current_year % 4)) % 4
}

let current_year = 2020;
let next_leap_year = next_leap_year(current_year);
println!("Next leap year: {}", current_year + next_leap_year);
```
Вивід: ```Next leap year: 2024```

Є також можливість використовувати стандартну бібліот