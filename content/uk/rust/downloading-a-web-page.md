---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?

Завантаження веб-сторінки означає отримання її HTML-коду за допомогою програми. Програмісти роблять це, коли потрібно зібрати або проаналізувати інформацію з веб-сторінки.

## Як це робити:

Отже, давайте спробуємо запитати веб-сторінку в Rust. Для цього нам знадобиться бібліотека reqwest зробити HTTP-запит.

```Rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
   let res = reqwest::get("https://www.example.com").await?;
   
   let body = res.text().await?;
   
   println!("body: {:?}", body);
   
   Ok(())
}
```

Виклик reqwest::get повертає `Response` в майбутньому, яке ми можемо асинхронно чекати. Виклик `text()` також повертає майбутнє, тому ми можемо отримати весь вміст респондента.

## Поглиблений погляд

Історично завантаження веб-сторінки було простим процесом, але з ростом складності веб-сторінок (JavaScript, AJAX тощо) стало складніше отримати всю інформацію лише одним HTTP-запитом. 

Щодо альтернатив, існують різні методи завантаження веб-сторінки, включаючи використання бібліотек, таких як LWP в Perl, urllib2 в Python, Net::HTTP в Ruby і т. д. 

Щодо подробиць виконання, reqwest використовує бібліотеку hyper для HTTP, а для HTTPS - rust-native-tls.

## Див. також:

[Reqwest documentation](https://docs.rs/reqwest/0.11.0/reqwest/)
[Tokio документація](https://docs.rs/tokio/)
[async-std документація](https://docs.rs/async-std/)