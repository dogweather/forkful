---
title:                "Haskell: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Звернення HTTP-запиту з базовою автентифікацією корисно для отримання доступу до захищених ресурсів, таких як приватні веб-сторінки або API-інтерфейси.

## Як це зробити

```Haskell
import Network.HTTP
import Network.URI
import Network.HTTP.Base (urlEncode)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as C (pack, unpack)

main :: IO ()
main = do
    -- Задаємо URL для запиту
    let url = "https://example.com/api/users"

    -- Перетворюємо URL у тип URI
    let uri = parseURI url

    -- Створюємо об\'єкт HTTP-запиту з методом GET
    let request = Request {rqURI = fromJust uri, rqMethod = GET, rqHeaders = [], rqBody = ""}

    -- Додаємо заголовки автентифікації
    let username = "my_username"
    let password = "my_password"
    let authString = C.unpack $ encode $ C.pack $ username ++ ":" ++ password
    let Authorization = C.pack $ "Basic " ++ authString
    let request' = request {rqHeaders = [("Authorization", Authorization)]}

    -- Виконуємо запит і друкуємо вивід
    response <- simpleHTTP request'
    body <- getResponseBody response
    putStrLn body
```

Приклад виводу запиту з базовою автентифікацією може виглядати так:

```
{"id": 1234, "name": "John Doe", "email": "johndoe@example.com"}
```

## Детальний огляд

Базова автентифікація передбачає включення заголовка "Authorization" у запиті, який містить базову64-кодовану комбінацію імені користувача та пароля, розділену двокрапкою. Цей заголовок потрібен для перевірки автентичності користувача. Детальну інформацію про базову автентифікацію можна знайти у [специфікації HTTP-протоколу](https://tools.ietf.org/html/rfc2617).

See Also:

- [Haskell HTTP бібліотека](https://hackage.haskell.org/package/HTTP)
- [HTTP-протокол специфікація](https://tools.ietf.org/html/rfc2616)
- [Як використовувати автентифікацію у Haskell](https://guide.aelve.com/haskell/use-http-client-authentication-663f9c48.html)