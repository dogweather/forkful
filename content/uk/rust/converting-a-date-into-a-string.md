---
title:    "Rust: Перетворення дати у рядок"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Чому

У програмуванні часто виникає необхідність перетворення дати у рядок, наприклад, для відображення на веб-сторінці або у звіті. У цій статті ми розглянемо, як це зробити за допомогою мови програмування Rust.

## Як це зробити

```Rust
use chrono::NaiveDate;

fn main() {
  // створимо змінну, що містить дату
  let date = NaiveDate::from_ymd(2020, 11, 25);
  
  // конвертуємо дату у рядок з використанням шаблону "дд-мм-рррр"
  let date_str = date.format("%d-%m-%Y").to_string();
  
  println!("{}", date_str); // виведе "25-11-2020"
}
```

У прикладі використано бібліотеку `chrono`, що дозволяє зручно працювати з датами та часом у Rust. Для перетворення дати у рядок ми використовуємо метод `format()` та задаємо шаблон, за яким потрібно вивести дату. Детальніше про можливі шаблони можна дізнатися у [документації](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html).

## Глибоке погруження

Крім стандартних шаблонів, бібліотека `chrono` також надає можливість створити свій власний шаблон для виведення дати. Наприклад, якщо нам потрібно вивести день тижня українською мовою, ми можемо використати наступний код:

```Rust
use chrono::NaiveDate;

fn main() {
  // створимо змінну, що містить дату
  let date = NaiveDate::from_ymd(2020, 11, 25);
  
  // створимо свій власний шаблон для виведення дня тижня українською мовою
  let ukr_weekday = date.format("%a").with_locale("uk");
  
  println!("{}", ukr_weekday); // виведе "Срд"
}
```

Цей приклад використовує метод `with_locale()` для встановлення потрібної локалі, у нашому випадку це українська мова. Більше про роботу з локалями у бібліотеці `chrono` можна дізнатися [тут](https://docs.rs/chrono/0.4.19/chrono/format/strftime/struct.StrftimeItems.html#method.with_locale).

## Дивись також

- [Документація з бібліотекою `chrono`](https://docs.rs/chrono/0.4.19/chrono/)
- [Створення власних шаблонів для виведення дати у бібліотеці `chrono`](https://docs.rs/chrono/0.4.19/chrono/format/strftime/struct.StrftimeItems.html)
- [Програмування на мові Rust](https://rust-lang.org/uk/)