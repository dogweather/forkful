---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?

Отправка HTTP-запиту із базовою аутентифікацією - це процес передачі користувацьких облікових даних (логіну та пароля) на сервер для аутентифікації. Програмісти роблять це, щоб надіслати засекречену інформацію або отримати доступ до захищених даних.

## Як це робити:

Отправка HTTP-запиту в Gleam починається з імпортування потрібного пакету `httpc`. Ось базовий приклад:

```gleam
import gleam/httpc
import gleam/bit_builder.{BitBuilder}
import gleam/http.{Headers}

fn basic_auth_header(username: String, password: String) -> Headers {
  let login_data = BitBuilder.append(username)
    |> BitBuilder.append(":")
    |> BitBuilder.append(password)
    |> BitBuilder.to_string
    |> base64:encode
    |> BitBuilder.from_string
  let header = httpc:headers([{<<"authorization">>, <<"Basic " + login_data>>}])
  header
}
```

У запиті використовується заголовок "İuthorization", до якого додається закодований в Base64 рядок "username:password".

## Поглиблений аналіз:

1. **Історичний контекст**: Протокол базової аутентифікації HTTP вперше з’явився у 1996 році в рамках специфікації HTTP/1.0.
2. **Альтернативи**: Існують різноманітні альтернативи базіковій аутентифікації, такі як оцифруваня паролів, OAuth, або ж JWT.
3. **Деталі реалізації**: У HTTP-запитах базова аутентифікація передається через заголовки. Значення заголовку - пароль і ім'я користувача, закодовані в base64, після ключового слова "Basic".

## Дивіться також:

Прочитайте документацію по модулю `httpc` в Gleam [тут](https://hexdocs.pm/gleam_httpc/readme.html). Ви також можете знайти інформацію про аутентифікацію в HTTP на сайті [MDN Web Docs](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication). Для детальнішого ознайомлення з аутентифікацією в Gleam, перегляньте цей [довідник](https://gleam.run/book/tour/http-requests.html).