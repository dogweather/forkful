---
title:                "Rust: Надсилання http запиту з основною аутентифікацією"
simple_title:         "Надсилання http запиту з основною аутентифікацією"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Протокол HTTP дозволяє нам взаємодіяти з веб-програмами та отримувати дані з Інтернету. Застосування автентифікації базовим методом допоможе забезпечити безпеку та захищеність наших HTTP-запитів.

## Як використовувати

```Rust
use reqwest::Client;

let client = Client::new();
let response = client.post("https://example.com")
    .basic_auth("username", Some("password"))
    .send()
    .await?;

println!("Status: {}", response.status());
```

 У цьому прикладі ми використовуємо бібліотеку `reqwest` для створення клієнта HTTP та відправлення POST-запиту з базовою автентифікацією. Ми можемо також вказати ім'я користувача та пароль для доступу до ресурсу.

## Глибоке дослідження

HTTP автентифікація базовим методом полягає в тому, що сервер запитує у користувача логін та пароль для авторизації доступу до ресурсу. Ці дані потім передаються у заголовку запиту `Authorization` у форматі `Basic base64_encoded(username:password)`. Для нашого прикладу це буде виглядати як `Basic dXNlcm5hbWU6cGFzc3dvcmQ=`.

## Дивіться також

- [Документація бібліотеки reqwest](https://docs.rs/reqwest/0.11.4/reqwest/)
- [Стаття про HTTP автентифікацію](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)
- [Приклад використання базової автентифікації в Rust](https://medium.com/@mattanden/rust-http-client-tutorial-be5d22192d86)