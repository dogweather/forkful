---
title:                "Аналіз HTML"
aliases:
- /uk/rust/parsing-html.md
date:                  2024-02-03T19:13:10.836353-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Парсинг HTML у Rust полягає у вилученні даних із HTML-документів, що є необхідним для веб-скрапінгу, екстракції даних або створення веб-павуків. Програмісти роблять це для автоматизації збору інформації з вебу, аналізу веб-контенту або міграції контенту з однієї платформи на іншу.

## Як це робити:

Для парсингу HTML у Rust вам часто знадобиться використовувати крейт `scraper`, який надає високорівневий інтерфейс для перегляду і маніпулювання HTML-документами.

Спочатку додайте `scraper` до вашого `Cargo.toml`:

```toml
[dependencies]
scraper = "0.12.0"
```

Далі, ось простий приклад, який вилучає всі URL-посилань із заданого HTML-рядка:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Посилання 1</a>
        <a href="http://example.com/2">Посилання 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("Знайдено посилання: {}", link);
    }
}
```

Вивід:

```
Знайдено посилання: http://example.com/1
Знайдено посилання: http://example.com/2
```

У цьому прикладі ми парсимо простий HTML-документ для знаходження всіх елементів `<a>` та вилучення їх атрибутів `href`, ефективно друкуємо URL-адреси всіх посилань у документі. Бібліотека `scraper` спрощує парсинг HTML і вибір конкретних елементів за допомогою CSS-селекторів, роблячи її ідеальною для завдань веб-скрапінгу в Rust.
