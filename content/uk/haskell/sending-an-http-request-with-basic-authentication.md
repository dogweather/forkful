---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

```
## Що й чому?

Відправлення HTTP-запиту з базовою автентифікацією це, по суті, спосіб забезпечення безпечного обміну даними між сервером і клієнтом. Програмісти використовують це для захисту чутливої інформації та перевірки користувацьких прав доступу.

## Як це робити:

Робимо це за допомогою пакета `http-conduit`. Є кілька кроків:

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client (applyBasicAuth)
import Data.ByteString.Char8 (pack)

main :: IO ()
main = do
    request <- parseRequest "http://httpbin.org/basic-auth/user/passwd"
    let request' = applyBasicAuth (pack "user") (pack "passwd") request
    response <- httpLBS request'
    print $ getResponseStatus response
```

Це задає імена користувача та пароль, як "user" та "passwd" відповідно, та відправляє запит до сервера.

## Поглиблений розгляд:

Відправлення HTTP-запиту із базовою автентифікацією є старим та перевіреним стратегічним варіантом автентифікації. Проте на сьогоднішній день більш безпечними та широко використовуваними є такі альтернативи як OAuth та JWT.

Використання `applyBasicAuth` з пакету `http-client` просто укладає облікові дані в заголовок запиту "Authorization" в форматі, визначеному Internet Engineering Task Force для базової автентифікації HTTP.

## Дивіться також:

- HTTP Conduit: https://hackage.haskell.org/package/http-conduit
- Протокол автентифікації: https://tools.ietf.org/html/rfc2617
```