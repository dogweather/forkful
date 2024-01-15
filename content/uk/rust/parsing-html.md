---
title:                "Розбір html"
html_title:           "Rust: Розбір html"
simple_title:         "Розбір html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Для чого

Розбір HTML є важливою задачею для багатьох програм, таких як веб-скрапінг і парсінг вмісту сторінок. Використання мови програмування Rust може спростити цей процес і забезпечити ефективне та швидке рішення.

## Як це зробити

Для початку нам потрібно встановити мову програмування Rust і його залежності. Після цього ми можемо приступити до написання коду.

```Rust
// Підключення необхідного пакету
use scraper::{Html, Selector};

// Створення змінної з HTML-кодом
let html = r#"<div><h1>Hello World</h1></div>"#;

// Створення селектора для вибору елемента
let selector = Selector::parse("h1").unwrap();

// Використання селектора для отримання потрібного елемента
let element = Html::parse_document(html).select(&selector).next().unwrap();

// Виведення вмісту елемента
println!("Вміст елемента: {}", element.inner_html());
```

В результаті ми отримаємо такий вивід:

```
Вміст елемента: Hello World
```

## Глибше занурення

Одним з головних інструментів для парсінгу HTML в Rust є пакет scraper. Він надає зручний інтерфейс для роботи з HTML-кодом та селекторами, що дозволяє ефективно витягувати потрібні елементи зі сторінки. Також існують інші пакети, такі як select, які надають можливості для роботи з HTML в Rust.

## Дивіться також

- [Документація з пакету scraper](https://docs.rs/scraper/)
- [Приклади використання пакету scraper](https://github.com/lotabout/scraper/tree/master/examples)
- [Документація з пакету select](https://docs.rs/select/)
- [Репозиторій мови Rust на GitHub](https://github.com/rust-lang/rust)