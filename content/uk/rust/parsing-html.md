---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Що і Навіщо?

Парсинг HTML - це процес вилучення конкретних даних з HTML-документів. Програмісти це роблять для високої автоматизації обробки веб-контенту.

## Як це робити:

Бібліотека `scraper` дозволяє легко парсити HTML у Rust. Давайте подивимося на простий приклад:

```Rust
use scraper::{Html, Selector};

fn main() {
  let html_doc = r#"
    <html>
      <body>
        <h1>Hello, World!</h1>
      </body>
    </html>
  "#;

  let fragment = Html::parse_document(&html_doc);
  let selector = Selector::parse("h1").unwrap();

  for element in fragment.select(&selector) {
    let text = element.text().collect::<Vec<_>>();
    println!("{:?}", text);
  }
}
```
В результаті ви отримаєте наступне:

```Rust
["Hello, World!"]
```

## Поглиблений Занурення

Історично, парсинг HTML здебільшого використовувався для веб-скрапінгу і індексації. Однак, з появою сучасних технологій як, наприклад, JSON API, потреба в HTML-парсингу зменшилась.

Варто відмітити, що у вас є багато альтернатив для парсинга HTML. У Rust варто розглянути бібліотеки, такі як `html5ever` та `xml5ever`.

Щодо деталей реалізації, `scraper` використовує `html5ever` під капотом для синтаксичного аналізу HTML. Він також надає удобний API для вибору елементів за допомогою CSS-селекторів.

## Дивись Також

- Документація `scraper`: [https://docs.rs/scraper](https://docs.rs/scraper)
- Документація `html5ever`: [https://docs.rs/html5ever](https://docs.rs/html5ever)
- Документація `xml5ever`: [https://docs.rs/xml5ever](https://docs.rs/xml5ever)
- Специфікація HTML5: [https://www.w3.org/TR/html5/](https://www.w3.org/TR/html5/)