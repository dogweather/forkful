---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Получення поточної дати - це процес виявлення дати та часу в момент запуску програми. Програмісти використовують це для ведення журналів, створення штампів часу, відслідковування подій тощо.

## Як це?

```Rust 
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("{}", now);
}
```

Виконавши цей код, ви отримаєте поточну дату та час у форматі `YYYY-MM-DD HH:MM:SS.SSS UTC`. 

## Поглиблений матеріал 

Отримання поточної дати та часу в Rust історично вимагало звернення до бібліотеки стандартної системи часу, але з розробкою бібліотеки `chrono` стало набагато простіше. 

Як альтернативу, ви також можете використовувати бібліотеку `time`, проте `chrono` надає більше можливостей. 

Володіючи `chrono`, ви можете також легко конвертувати дати та час між різними часовими зонами, робити періоди часу та дати, використовуючи синтаксис на основі більш простих операторів. 

## Див. також 

- [Документація Chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [Проект Chrono на GitHub](https://github.com/chronotope/chrono)
- [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/)