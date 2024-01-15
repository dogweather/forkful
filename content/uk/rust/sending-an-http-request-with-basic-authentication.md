---
title:                "Відправка запиту http з базовою аутентифікацією"
html_title:           "Rust: Відправка запиту http з базовою аутентифікацією"
simple_title:         "Відправка запиту http з базовою аутентифікацією"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

![Rust Logo](https://www.rust-lang.org/static/images/rust-logo-blk.svg)

## Чому

Якщо вам потрібно отримати доступ до захищеного веб-ресурсу через вашу програму на Rust, вам потрібно буде використовувати базову аутентифікацію. Це забезпечить безпечну передачу облікових даних і дозволить вам зробити запит на сервер.

## Як

Для початку, необхідно імпортувати необхідні бібліотеки:
```Rust
use std::io::Read;
use reqwest::blocking::{Client, Response};
```

Потім ініціалізуйте змінну клієнта, яка буде використовуватись для виконання запиту:
```Rust
let client = Client::new();
```

Створіть новий запит, вказавши необхідний URL і метод запиту (у цьому випадку GET):
```Rust
let mut request = client.get("https://example.com").send().unwrap();
```

Далі, встановіть заголовки для запиту, включаючи заголовки для базової аутентифікації, використовуючи ваші облікові дані:
```Rust
request = request.basic_auth("username", Some("password"));
```

Нарешті, виконайте запит і отримайте відповідь з сервера:
```Rust
let mut response: Response = request.send().unwrap();
```

Ось приклад результуючого коду:
```Rust
use std::io::Read;
use reqwest::blocking::{Client, Response};

let client = Client::new();
let mut request = client.get("https://example.com").send().unwrap();
request = request.basic_auth("username", Some("password"));
let mut response: Response = request.send().unwrap();

let mut body = String::new();
response.read_to_string(&mut body);

println!("Status Code: {}", response.status());
println!("Response Body: {}", body);
```

В результаті, ви отримаєте відповідь з сервера з кодом статусу та тілом відповіді.

## Глибокий погляд

В даному прикладі ми використовували бібліотеку `reqwest` для створення запиту та обробки відповіді. Ця бібліотека забезпечує безпечну передачу даних від і до сервера.

Також важливо знати, що базова аутентифікація не є надійним методом аутентифікації, оскільки дані передаються у відкритому вигляді. Кращим варіантом буде використання більш сучасних методів аутентифікації, таких як OAuth.

## Дивись також

- [Документація по бібліотеці reqwest](https://docs.rs/reqwest/)
- [Приклади використання базової аутентифікації в Rust](https://github.com/reqwest-rs/reqwest#authentication)
- [Безпека веб-запитів в Rust](https://dev.to/anshulgupta11/how-to-authenticate-a-web-request-in-rust-2lea)