---
title:                "Надсилання http-запиту з базовою аутентифікацією"
html_title:           "Arduino: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та чому?

Надсилання HTTP-запиту із базовою аутентифікацією - це процес передачі логіна та паролю в HTTP-запиті. Програмісти роблять це для отримання доступу до захищених даних на сервері.

## Як це зробити:

Rust має потужну бібліотеку для роботи з HTTP-запитами під назвою `reqwest`. Ось приклад використання:

```Rust
use reqwest::header;
use std::collections::HashMap;

let mut headers = header::HeaderMap::new();
headers.insert("Authorization", header::HeaderValue::from_static("Basic QWxhZGRpbjpPcGVuU2VzYW1l"));

let client = reqwest::Client::builder()
    .default_headers(headers)
    .build()?;

let res = client.get("https://httpbin.org/get")
    .send()
    .await?;

println!("{}", res.status());
```

Цей код створює HTTP-клієнт з заголовком "Authorization", який використовує Base64-кодування для створення значення "Basic QWxhZGRpbjpPcGVuU2VzYW1l" та надсилає запит до "https://httpbin.org/get". Відповідь виводиться в консоль.

## Глибше занурення

Надсилання HTTP-запиту із базовою аутентифікацією було створено у 1996 році як частина HTTP/1.0. Це основний спосіб надсилання оброблених даних користувача на веб-сервер. 

Але є інші методи аутентифікації, такі як OAuth, що надає більше гнучкості та безпеки.

Щодо виконання коду, значення заголовка "Authorization" кодується Base64 та додається до HTTP-запиту. Будь-який сервер, що розуміє заголовок "Authorization", зможе його розкодувати та використати для аутентифікації користувача.

## Дивіться також

[Документація `reqwest`](https://docs.rs/reqwest)
[Основи HTTP-аутентифікації](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
[Базова аутентифікація в Wikipedia](https://uk.wikipedia.org/wiki/%D0%91%D0%B0%D0%B7%D0%BE%D0%B2%D0%B0_%D0%B0%D0%B2%D1%82%D0%B5%D0%BD%D1%82%D0%B8%D1%84%D1%96%D0%BA%D0%B0%D1%86%D1%96%D1%8F)