---
title:                "Gleam: Надсилання запиту http з базовою автентифікацією"
simple_title:         "Надсилання запиту http з базовою автентифікацією"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Однією з ключових функцій мови програмування Gleam є можливість надсилати HTTP-запити з базовою аутентифікацією. Це дає можливість забезпечити безпеку вашого додатку та взаємодію з потрібними сервісами за допомогою простого та ефективного методу.

## Як

Для надсилання HTTP-запиту з базовою аутентифікацією в Gleam, спочатку необхідно підключити бібліотеку ```httpc``` за допомогою директиви "```import```". Далі, можна скористатися функцією ```httpc.request()``` для відправки запиту, вказавши метод запиту, URL та параметри аутентифікації у вигляді кортежу з логіном та паролем. Приклад коду та виведення наведено нижче:

```Gleam
import httpc

request =
  httpc.request(
    method: "GET",
    url: "https://example.com/api",
    auth: ("username", "password")
  )

case request do
  Ok(response) -> response.body
  Error(_) -> "Something went wrong"
end
```

Виведення: "Зміст відповіді на запит GET до https://example.com/api"

## Глибші деталі

При надсиланні HTTP-запиту з базовою аутентифікацією, Gleam встановлює заголовок "Authorization" зі значенням "Basic", зашифрованого у форматі Base64 від кортежу логіну та паролю, вказаних у параметрах аутентифікації. Це дозволяє здійснити успішну аутентифікацію та отримати доступ до захищених даних.

## Дивись також

- [Офіційна документація Gleam](https://gleam.run/documentation/)
- [Розділ про HTTP-запити у документації Gleam](https://gleam.run/documentation/http/)
- [Бібліотека httpc для Gleam](https://hex.pm/packages/httpc)