---
title:                "Rust: Розрахунок дати в майбутньому чи минулому."
simple_title:         "Розрахунок дати в майбутньому чи минулому."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні багато різноманітних завдань, і одним з них є обчислення дати у майбутньому або минулому. Це може бути необхідно для багатьох цілей, таких як розробка програм для планування, обліку часу або просто задоволення цікавості.

## Як це зробити

Для обчислення дати у майбутньому та минулому у мові програмування Rust, ми можемо скористатися різними функціями та типами даних.

```Rust
// Обчислення дати у майбутньому
use chrono::{Utc, Duration};

let now = Utc::now();
let next_week = now + Duration::weeks(1);
println!("Наступного тижня буде {}", next_week);

// Обчислення дати у минулому
use chrono::{Utc, Duration};

let now = Utc::now();
let last_week = now - Duration::weeks(1);
println!("Минулого тижня було {}", last_week);
```

В результаті виконання цього коду ми отримаємо дати, відповідно, наступного тижня та минулого тижня.

## Глибше

Для виконання обчислень дат у майбутньому або минулому, потрібно користуватися бібліотекою `chrono`. Вона дозволяє нам працювати з датами та часом, а також виконувати різноманітні операції з ними, включаючи обчислення дати у майбутньому або минулому за допомогою методу `+` або `-`.

Також можна використовувати інші функції та методи бібліотеки `chrono`, такі як `DateTime::with_timezone()` або `Duration::days()`, щоб отримувати більш точний результат обчислень.

## Дивись також

- Документація по бібліотеці `chrono`: [https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)
- Руководство з використання бібліотеки `chrono`: [https://dev.to/joaoh82/how-to-use-chrono-date-time-in-rust-48l0](https://dev.to/joaoh82/how-to-use-chrono-date-time-in-rust-48l0)