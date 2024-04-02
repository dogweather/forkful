---
date: 2024-01-20 18:02:47.704907-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2014 \u0446\u0435 \u0442\u0435\u0445\u043D\u0456\u043A\u0430, \u044F\u043A\u0430\
  \ \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0438\u043A\u043E\u043D\
  \u0443\u0432\u0430\u0442\u0438 \u0431\u0435\u0437\u043F\u0435\u0447\u043D\u0438\u0439\
  \ \u0434\u043E\u0441\u0442\u0443\u043F \u0434\u043E \u0432\u0435\u0431-\u0440\u0435\
  \u0441\u0443\u0440\u0441\u0456\u0432 \u0437 \u0456\u043C\u0435\u043D\u0435\u043C\
  \ \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0447\u0430 \u0442\u0430\
  \ \u043F\u0430\u0440\u043E\u043B\u0435\u043C.\u2026"
lastmod: '2024-03-13T22:44:48.935802-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2014 \u0446\u0435 \u0442\u0435\u0445\u043D\u0456\u043A\u0430, \u044F\u043A\u0430\
  \ \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0438\u043A\u043E\u043D\
  \u0443\u0432\u0430\u0442\u0438 \u0431\u0435\u0437\u043F\u0435\u0447\u043D\u0438\u0439\
  \ \u0434\u043E\u0441\u0442\u0443\u043F \u0434\u043E \u0432\u0435\u0431-\u0440\u0435\
  \u0441\u0443\u0440\u0441\u0456\u0432 \u0437 \u0456\u043C\u0435\u043D\u0435\u043C\
  \ \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0447\u0430 \u0442\u0430\
  \ \u043F\u0430\u0440\u043E\u043B\u0435\u043C.\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
