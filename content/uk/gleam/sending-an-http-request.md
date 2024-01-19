---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що й Навіщо?

Надсилання HTTP-запиту - це процес комунікації з веб-сервером: ви "запитуєте" якусь інформацію та очікуєте відповідь. Програмісти роблять це, щоб взаємодіяти з API, отримувати дані, відправляти дані тощо.

## Як це робити:

Ось приклад того, як надіслати HTTP-запит у Gleam:

```Gleam
import gleam/httpc
import gleam/http.{Get}

let request = Get("https://httpbin.org/json")
httpc.send(request)
```

Виправлення помилок може виглядати так:

```Gleam
case httpc.send(request) {
  Ok(response) -> response
  Error(e) -> e
}
```

## Поглиблено

Надсилання HTTP-запитів - це сложена практика, корінні якої сягають початків інтернету. Наявність різноманітних методів (POST, GET, DELETE, і т.д.) та типів контенту сприяє гнучкості HTTP-запитів.

Як альтернативу HTTP-запитам, можна використовувати більш новітький та замудрений протокол gRPC, але він може бути замудреним для простих завдань.

Дійсно, Gleam використовує під капотом Erlang/OTP для отправки HTTP-запитів, що гарантує високу продуктивність та надійність.

## Див. також

* [Документація Gleam по httpc](https://hexdocs.pm/gleam_httpc/gleam/httpc/index.html)
* [Докладніше про HTTP-запити](https://developer.mozilla.org/uk/docs/Web/HTTP/Overview)
* [Порівняння між HTTP-запитами та gRPC](https://www.ionos.com/digitalguide/websites/web-development/grpc/)