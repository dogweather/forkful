---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та чому?
Надсилання HTTP-запиту із базовою аутентифікацією - це метод, що дозволяє забезпечити доступ до захищених ресурсів в мережі. Програмісти використовують цей метод, щоб забезпечити безпеку даних користувачів і їх конфіденційність.

## Як це зробити?
У Elm (остання версія) для надсилання HTTP-запиту з базовою аутентифікацією вам буде потрібна бібліотека `elm/http`. 

```Elm
import Http
import Base64

basicAuth : String -> String -> Http.Header
basicAuth username password =
  let
    credentials =
      Base64.encode (username ++ ":" ++ password)
  in
  Http.header "Authorization" ("Basic " ++ credentials)

myRequest : Http.Request String
myRequest = 
  Http.request 
  { method = "GET"
  , headers = [ basicAuth "my_username" "my_password" ]
  , url = "https://my-api-url.com"
  , body = Http.emptyBody
  , expect = Http.expectString identity
  , timeout = Nothing
  , tracker = Nothing 
  }   
```
Вивід:
```Elm
{
  method = "GET",
  headers = [Authorization: "Basic bXlfdXNlcm5hbWU6bXlfcGFzc3dvcmQ="],
  url = "https://my-api-url.com",
  body = "",
  expect = identity,
  timeout = Nothing,
  tracker = Nothing
}
```
## Поглиблений розбір
Базова аутентифікація була однією з перших форм аутентифікації у вебі, але з часом була замінена безпечнішими методами, такими як OAuth.

Альтернатива базовій аутентифікації - це «маркери доступу», які зазвичай використовуються з OAuth. Вони дають більше контролю над тим, хто має доступ до яких ресурсів і на скільки довго.

Що стосується деталей реалізації, то Elm відправляє HTTP-запит із заголовком "Authorization", що містить закодовані в Base64 об'єднані ім'я користувача та пароль, передує "Basic".

## Дивіться також
- [Elm Http бібліотека](https://package.elm-lang.org/packages/elm/http/latest/)
- [Модуль Base64 Elm](https://package.elm-lang.org/packages/truqu/elm-base64/latest)
- [Протокол аутентифікації HTTP (MDN Web Docs)](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)