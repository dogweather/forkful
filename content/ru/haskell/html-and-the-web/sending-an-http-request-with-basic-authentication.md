---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:59.592358-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u0430\u043C \u043F\u043E\u043D\u0430\u0434\u043E\u0431\u0438\
  \u0442\u0441\u044F \u043F\u0430\u043A\u0435\u0442 `http-conduit` \u0434\u043B\u044F\
  \ HTTP-\u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0439 \u0438 `base64-bytestring`\
  \ \u0434\u043B\u044F \u043A\u043E\u0434\u0438\u0440\u043E\u0432\u0430\u043D\u0438\
  \u044F \u0443\u0447\u0435\u0442\u043D\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\
  \u0445. \u0418\u043C\u043F\u043E\u0440\u0442\u0438\u0440\u0443\u0439\u0442\u0435\
  \ \u0438\u0445 \u0438\u2026"
lastmod: '2024-03-13T22:44:45.133256-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u0430\u043C \u043F\u043E\u043D\u0430\u0434\u043E\u0431\u0438\u0442\
  \u0441\u044F \u043F\u0430\u043A\u0435\u0442 `http-conduit` \u0434\u043B\u044F HTTP-\u0434\
  \u0435\u0439\u0441\u0442\u0432\u0438\u0439 \u0438 `base64-bytestring` \u0434\u043B\
  \u044F \u043A\u043E\u0434\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F \u0443\
  \u0447\u0435\u0442\u043D\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

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
