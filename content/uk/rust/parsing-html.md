---
title:                "Аналіз html"
html_title:           "Rust: Аналіз html"
simple_title:         "Аналіз html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Що & Чому?
Парсинг HTML - це процес аналізування та інтерпретації HTML-документів. Програмісти використовують цей процес для отримання даних з веб-сторінок і подальшого їх оброблення.

## Як це зробити:
```Rust
use html5ever::{parse_document, tendril::TendrilSink};

let html = "<html><body><p>Hello, world!</p></body></html>";

let dom = parse_document(Default::default()).from_utf8().read_from(html.as_bytes()).unwrap();
let root = dom.document;

for child in root.descendants() {
    match child.data() {
        Element { name, .. } => println!("{}", name.local),
        Text { contents, .. } => println!("{}", contents.borrow()),
        _ => (),
    }
}
```

Вивід: `html, body, p, Hello, world!`

## Глибокий занурення:
1. Історичний контекст: Парсинг HTML з'явився з появою перших веб-сторінок і є важливою частиною веб-розробки.
2. Альтернативи: Існують інші мови програмування, такі як Python і JavaScript, які також мають бібліотеки для парсингу HTML.
3. Деталі реалізації: У Rust для парсингу HTML використовується бібліотека html5ever, яка підтримує стандарти HTML, такі як HTML5 та XHTML.

## Дивіться також:
- Документація html5ever: https://docs.rs/html5ever/0.24.0/html5ever/
- Приклади парсингу HTML на Rust: https://github.com/kbknapp/html5ever-examples-rs