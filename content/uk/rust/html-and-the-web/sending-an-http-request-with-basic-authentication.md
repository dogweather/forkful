---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/rust/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:47.704907-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це таке і Навіщо?
Відправка HTTP-запиту з базовою аутентифікацією — це техніка, яка дозволяє виконувати безпечний доступ до веб-ресурсів з іменем користувача та паролем. Програмісти використовують це для взаємодії з захищеними API, отримання даних та управління сервісами.

## Як це зробити:
```Rust
use reqwest::{blocking::Client, header};

fn main() {
    let username = "user";
    let password = "pass";
    let credentials = format!("{}:{}", username, password);
    let base64_credentials = base64::encode(credentials.as_bytes());

    let client = Client::new();
    let res = client.get("http://example.com")
        .header(header::AUTHORIZATION, format!("Basic {}", base64_credentials))
        .send();

    match res {
        Ok(response) => println!("Response Status: {}", response.status()),
        Err(e) => println!("HTTP Request Failed: {}", e),
    }
}
```
Sample output:
```
Response Status: 200 OK
```

## Глибший Занурення
Перед історією HTTP, аутентифікація часто була вбудована у самі програми. З часом, потреба у стандартизованому рішенні призвела до включення базової аутентифікації до HTTP 1.0. Альтернативи, як OAuth та API ключі, забезпечують більш безпечні варіанти аутентифікації але вимагають додаткової налаштування та логіки. При використанні базової аутентифікації, credentials (зазвичай ім'я користувача та пароль) кодуються у Base64 і додаються до заголовку `Authorization` HTTP-запиту. Це просте рішення, яке досі іноді корисне, але пам'ятайте, що воно не шифрує ваші дані.

## Дивіться Також
- RFC 7617, "The 'Basic' HTTP Authentication Scheme": https://tools.ietf.org/html/rfc7617
- Reqwest, an ergonomic Rust HTTP Client: https://docs.rs/reqwest/
- base64 Encoding in Rust: https://docs.rs/base64/
- Mozilla Developer Network - HTTP authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
