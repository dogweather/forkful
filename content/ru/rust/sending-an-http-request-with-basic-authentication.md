---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
date:                  2024-01-29T00:03:14.212878-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"

category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса с базовой аутентификацией подразумевает добавление в заголовок запроса имени пользователя и пароля для подтверждения вашего доступа. Это делается, когда сервисам необходимо убедиться, что доступ пытаетесь получить именно вы, а не какой-то Joe Schmoe, пытающийся получить доступ к чему-то.

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
