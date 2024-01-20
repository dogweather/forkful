---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що та чому?

Порівняння двох дат - це процес констатування, яка дата є більш ранньою, або чи вони однакові. Це необхідно для обробки часових відступів, слідування подій у правильному порядку та інших розрахунків.

## Як зробити:

```Rust
use chrono::{DateTime, NaiveDateTime, Utc};

fn main() {
    let start = DateTime::<Utc>::from_utc(NaiveDateTime::from_timestamp(61, 0), Utc);
    let end = start.checked_add_signed(chrono::Duration::seconds(120)).unwrap();
    
    if end > start {
        println!("end is after start");
    } else if end < start {
        println!("end is before start");
    } else {
        println!("end is at the same time as start");
    }
}
```

Цей код виведе `"end is after start"`, адже дата `end` була створена, додавши 120 секунд до дати `start`.

## Пірнення углиб:

1. Історичний контекст: у минулому програмісти повинні були самостійно обробляти всю проблематику дати і часу. З технічної точки зору, це могло бути дуже складно і нестабільно. Мова Rust надає природній і безпечний спосіб робити це за допомогою бібліотеки Chrono.

2. Альтернативи: існують інші підходи до порівняння дат, наприклад, обчислення різниці між двома датами та перевірка, чи цей результат позитивний, негативний, чи нуль.

3. Деталі реалізації: у мові Rust, порівняння двох дат відбувається на рівні системи за допомогою операцій з числами. Високорівневі функції бібліотеки Chrono (як `checked_add_signed`) використовують цей примітивний механізм для надання більш зручного та безпечного інтерфейсу.

## Дивіться також:

2. [Бібліотека Chrono Rust](https://docs.rs/chrono/0.4.11/chrono/)