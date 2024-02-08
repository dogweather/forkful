---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
aliases:
- ru/haskell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-29T00:02:59.592358-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Отправка HTTP-запроса с базовой аутентификацией означает, что ваша программа "стучится в дверь" веб-сервиса, передавая имя пользователя и пароль для входа. Программисты делают это для доступа к API, которые закрыты для общего доступа, или для выполнения действий от имени пользователя.

## Как это сделать:
Вам понадобится пакет `http-conduit` для HTTP-действий и `base64-bytestring` для кодирования учетных данных. Импортируйте их и используйте `applyBasicAuth` для добавления учетных данных в ваш запрос.

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (encode)

-- Составляем заголовок базовой аутентификации
let username = "user"
let password = "pass"
let auth = encode $ pack (username ++ ":" ++ password)

-- Создаем запрос
request' = parseRequest_ "GET http://example.com/secret"
let request = setRequestHeader "Authorization" ["Basic " <> auth] request'

-- Выполняем запрос
response <- httpLBS request

-- Обрабатываем ответ
print $ getResponseBody response
```

Это выведет ответ API, если ваши учетные данные верны.

## Глубокое погружение
Базовая аутентификация — древняя на мерки интернета, разработана в начале 90-х, и она настолько проста, как только возможно: base64 кодированное `username:password` отправляется в заголовке. В ней отсутствуют изысканные функции, такие как истечение срока действия токена, и, поскольку она не шифруется, ее всегда следует использовать через HTTPS.

Альтернативы, такие как OAuth, обеспечивают более безопасный и детализированный контроль. Для Haskell библиотеки вроде `http-client` и `wreq` предлагают больше вариантов и гибкости.

С точки зрения реализации, помните, что не следует хранить учетные данные жестко закодированными! Используйте переменные среды или безопасное хранилище в производственной среде. И поскольку кодирование `base64` не является шифрованием (любой может его декодировать), использование HTTPS не просто хорошая идея, это обязательно.

## Смотрите также
- Документация Haskell `http-conduit`: https://hackage.haskell.org/package/http-conduit
- `base64-bytestring` для кодирования: https://hackage.haskell.org/package/base64-bytestring
- Для улучшенной безопасности читайте про OAuth2 на Haskell: https://hackage.haskell.org/package/hoauth2
- Читайте о лучших практиках хранения секретов: https://www.yesodweb.com/book/security-considerations
