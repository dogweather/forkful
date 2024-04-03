---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:14.212878-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0434\u043E\u0431\u0430\
  \u0432\u044C\u0442\u0435 \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C\u044B\
  \u0435 \u0437\u0430\u0432\u0438\u0441\u0438\u043C\u043E\u0441\u0442\u0438 \u0432\
  \ \u0444\u0430\u0439\u043B `Cargo.toml`."
lastmod: '2024-03-13T22:44:44.665403-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0434\u043E\u0431\u0430\u0432\
  \u044C\u0442\u0435 \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C\u044B\u0435\
  \ \u0437\u0430\u0432\u0438\u0441\u0438\u043C\u043E\u0441\u0442\u0438 \u0432 \u0444\
  \u0430\u0439\u043B `Cargo.toml`."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
Сначала добавьте необходимые зависимости в файл `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
base64 = "0.13"
```

Теперь вот код на Rust для отправки GET-запроса с базовой аутентификацией:

```rust
use reqwest::header::{Authorization, Basic};
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let user = "Aladdin";
    let password = "open sesame";
    
    let auth = Basic {
        username: user.into(),
        password: Some(password.into()),
    };
    
    let response = client
        .get("http://example.com/secrets")
        .header(Authorization(auth))
        .send()
        .await?;
    
    let content = response.text().await?;
    println!("Ответ: {}", content);
    
    Ok(())
}
```

Если всё правильно, то будут напечатаны секреты. Вы поняли основную идею.

## Подробнее
До появления `reqwest` люди мучились с `curl` на Rust. Это как предпочесть ручную пилу электрической. Базовая аутентификация, хоть и кажется простой, не является крепостью типа Форт-Нокс. Это всего лишь Base64 от "имя пользователя:пароль", без шифрования, поэтому HTTPS является обязательным.

Альтернативы? OAuth 2.0 значительно опережает базовую аутентификацию, предлагая токены вместо конкретных учетных данных. Тем не менее, это сложнее. Есть также аутентификация Bearer, которая держит токены как секретный рукопожатие.

Под капотом, `reqwest` является высокоуровневым HTTP-клиентом, хорошо работающим с асинхронными возможностями Rust. Структура 'Basic' создает заголовок, 'Authorization' вставляет его, и вуаля, вы стучитесь в дверь сервера с секретным шепотом.

## Смотрите также
Для большего количества знаний и магии:

- документация reqwest: [https://docs.rs/reqwest](https://docs.rs/reqwest)
- Понимание базовой аутентификации HTTP: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Асинхронное программирование на Rust: [https://rust-lang.github.io/async-book/](https://rust-lang.github.io/async-book/)
- документация крейта base64 на Rust: [https://docs.rs/base64](https://docs.rs/base64)
