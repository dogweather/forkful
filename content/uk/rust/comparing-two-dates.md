---
title:    "Rust: Порівняння двох дат"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Існує багато ситуацій, коли програмістам необхідно порівняти дві дати. Наприклад, для перевірки терміну дії ліцензії або для виведення даної у заданому форматі. В цьому блозі ми розглянемо, як порівняти дві дати в мові програмування Rust.

## Як це зробити

Найпростіший спосіб порівняти дві дати в Rust - використовуючи стандартну бібліотеку. Давайте створимо дві константи з датами: 
```Rust
static DATE_1: &str = "2021-10-15";
static DATE_2: &str = "2021-10-20";
```
Тепер ми можемо використовувати функцію `parse` із модуля `chrono` для перетворення цих значень у тип `DateTime<Local>`, який дозволяє порівнювати дати:
```Rust
use chrono::{DateTime, Local, NaiveDate, ParseResult, Timelike};

fn main() {
    let date1 = parse_date(DATE_1).unwrap();
    let date2 = parse_date(DATE_2).unwrap();
    
    if date1 < date2 {
        println!("Дата {} наступна після дати {}", DATE_2, DATE_1);
    } else if date2 < date1 {
        println!("Дата {} наступна після дати {}", DATE_1, DATE_2);
    } else {
        println!("Обидві дати однакові");
    }
}

fn parse_date(date: &str) -> ParseResult<DateTime<Local>> {
    let parsed_date = NaiveDate::parse_from_str(date, "%Y-%m-%d");
    parsed_date.map(|date| DateTime::<Local>::from(date.and_hms(0, 0, 0)))
}
```

В результаті ми отримаємо рядок `Дата 2021-10-20 наступна після дати 2021-10-15`.

## Глибоке дослідження

Порівняння дат у мові Rust засноване на порівнянні типу `DateTime`. Якщо ми взнати кількість секунд від початку часу у форматі Unix, то ми зможемо легко порівнювати дати. Наприклад, давайте порівняємо дві дати у форматі Unix:
```Rust
use chrono::{NaiveDateTime, Timelike, Utc};

fn main() {
    let date1 = NaiveDateTime::from_timestamp(1634313600, 0).timestamp();
    let date2 = NaiveDateTime::from_timestamp(1634673600, 0).timestamp();
    
    if date1 < date2 {
        println!("Дата 2021-10-20 наступна після дати 2021-10-15");
    } else if date2 < date1 {
        println!("Дата 2021-10-15 наступна після дати 2021-10-20");
    } else {
        println!("Обидві дати однакові");
    }
}
```

Якщо перетворити ці дати в повноформатний час у форматі `*DateTime<Local>*`, то буде видно, що друга дата є більшою за кількістю годин:
```Rust
if date1 < date2 {
    println!("Дата 2021-10-20 наступна після дати 2021-10-15");
} else