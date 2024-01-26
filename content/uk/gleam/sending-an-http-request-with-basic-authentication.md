---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:01:51.274438-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Що це таке і Навіщо?)
HTTP-запит із базовою аутентифікацією включає в себе відправлення імені користувача та пароля для доступу до ресурсу. Програмісти використовують це для захисту чутливих даних і обмеження доступу до ресурсів.

## How to: (Як це зробити:)
```Gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn send_authenticated_request() {
  let credentials = "username:password"
  let headers = [httpc.header("Authorization", "Basic " ++ base64.encode(credentials))]
  httpc.send(http.Request(base_url: "https://api.example.com", method: http.Get, headers: headers))
}

pub fn main() {
  assert Ok(response) = send_authenticated_request()
  response.status
  |> should.equal(200)
}

// Припустимо, що відповідь сервера:
// HTTP/1.1 200 OK
```

## Deep Dive (Занурення у Деталі)
Історично, базова аутентифікація - це один із методів HTTP аутентифікації, введений ще у 1996 році з HTTP/1.0. Її легко реалізувати, але вона не є найбезпечнішою. Інформація передається у відкритому вигляді, закодована в base64, що робить її вразливою до підслуховування. Тому зараз частіше використовують токени аутентифікації, такі як OAuth. У Gleam для HTTP-запитів з базовою аутентифікацією використовується модуль `httpc`. Пам'ятайте, що разом з базовою аутентифікацією слід використовувати HTTPS, щоб забезпечити шифрування даних.

## See Also (Дивіться також)
- Gleam HTTP documentation: https://hexdocs.pm/gleam_http/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- An article about HTTP authentication schemes: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
