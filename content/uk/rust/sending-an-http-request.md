---
title:                "Надсилання http-запиту"
html_title:           "Rust: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## За що

Надсилання HTTP запиту - це ключовий елемент веб-розробки та побудови мережевих додатків. Це необхідно для взаємодії з сервером та отримання необхідної інформації, такої як HTML сторінки та дані API. У Rust це можна зробити ефективно та безпечно, що робить його відмінним вибором для програмістів.

## Як

Надсилання HTTP запиту в Rust є простим завданням, завдяки пакету `reqwest`. Почнемо з встановлення пакету:

```rust
use reqwest::Error;
```

Тепер ми можемо створити функцію, яка буде надсилати запит та отримувати дані з сервера:

```rust
async fn send_request() -> Result<(), Error> {
    let resp = reqwest::get("https://www.example.com").await?;
    println!("Отримана відповідь: {:?}", resp.text().await?);
    Ok(())
}
```

Ця функція використовує асинхронний підхід, що дозволяє виконувати інші дії під час очікування на відповідь від сервера. Також ми можемо використати блок `match`, щоб обробити помилки та відображати повідомлення користувачу.

## Глибоке занурення

В Rust використання пакету `reqwest` дозволяє нам не тільки надсилати HTTP запити, але і налаштовувати їх з використанням різних параметрів. Наприклад, ми можемо додати заголовок до запиту:

```rust
let client = reqwest::Client::builder()
        .user_agent("Rust HTTP Request")
        .build()?;

let resp = client
        .get("https://www.example.com")
        .send()
        .await?;
```

Також ми можемо передавати дані з запитом, використовуючи метод `.post()`, або налаштувати параметри запиту, такі як таймаут та захист від перенаправлення.

## Дивись також

- [Документація пакету `reqwest`](https://docs.rs/reqwest/latest/reqwest/)
- [Стаття "Вчимося робити запити з HTTP у Rust"](https://medium.com/@joshuaellington/learning-how-to-make-http-requests-in-rust-f917043d627)
- [Форум Rust українською мовою](https://rust.in.ua/)