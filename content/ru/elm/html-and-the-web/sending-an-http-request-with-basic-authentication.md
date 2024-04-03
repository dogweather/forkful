---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:41.850293-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: Elm \u043E\
  \u0442\u043F\u0440\u0430\u0432\u043B\u044F\u0435\u0442 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u044B, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044F \u043F\
  \u0430\u043A\u0435\u0442 `Http`. \u0427\u0442\u043E\u0431\u044B \u0434\u043E\u0431\
  \u0430\u0432\u0438\u0442\u044C \u0431\u0430\u0437\u043E\u0432\u0443\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u044E,\
  \ \u0432\u044B \u043A\u043E\u0434\u0438\u0440\u0443\u0435\u0442\u0435 \u0443\u0447\
  \u0435\u0442\u043D\u044B\u0435 \u0434\u0430\u043D\u043D\u044B\u0435 \u0438 \u0432\
  \u043A\u043B\u044E\u0447\u0430\u0435\u0442\u0435 \u0438\u0445 \u0432\u2026"
lastmod: '2024-03-13T22:44:44.900896-06:00'
model: gpt-4-0125-preview
summary: "Elm \u043E\u0442\u043F\u0440\u0430\u0432\u043B\u044F\u0435\u0442 HTTP-\u0437\
  \u0430\u043F\u0440\u043E\u0441\u044B, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u044F \u043F\u0430\u043A\u0435\u0442 `Http`."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как сделать:
Elm отправляет HTTP-запросы, используя пакет `Http`. Чтобы добавить базовую аутентификацию, вы кодируете учетные данные и включаете их в заголовки запроса.

```Elm
import Http
import Base64

type alias Model = { ... }
type Msg = HttpRequestCompleted (Result Http.Error String)

-- Кодирование имени пользователя и пароля
basicAuthHeader : String -> String -> Http.Header
basicAuthHeader username password =
    let
        credentials = username ++ ":" ++ password
        encodedCredentials = Base64.encode credentials
    in
    Http.header "Authorization" ("Basic " ++ encodedCredentials)

-- Осуществление HTTP-запроса
sendRequestWithBasicAuth : Cmd Msg
sendRequestWithBasicAuth =
    let
        url = "https://example.com/protected/resource"
        request =
            Http.request
                { method = "GET"
                , headers = [ basicAuthHeader "myUsername" "myPassword" ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectString (HttpRequestCompleted)
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Http.send HttpRequestCompleted request
```

Когда вышеуказанная функция вызывается, Elm выполняет GET-запрос к указанному URL с заголовком Authorization, установленным в закодированные имя пользователя и пароль.

## Подробнее
Подход Elm к HTTP-запросам отражает общую философию языка: безопасность, легкость в обслуживании и понимание. Пакет `Http` инкапсулирует запросы таким образом, что они соответствуют архитектуре Elm.

Базовая аутентификация существует столько же, сколько и веб, являясь частью первоначальной спецификации HTTP (RFC 7617). Она проста, но не очень безопасна, поскольку учетные данные кодируются только в base64, но не шифруются. Поэтому критически важно использовать HTTPS для кодирования передачи.

Альтернативы базовой аутентификации включают OAuth, токены, такие как JWT, или API-ключи, каждый из которых предлагает повышенную сложность и улучшенную безопасность. Elm также поддерживает эти методы, но часто требуется дополнительные пакеты или пользовательские кодировщики и декодировщики.

## Смотрите также
- Официальная документация по пакету `Http` Elm: [package.elm-lang.org/packages/elm/http/latest](https://package.elm-lang.org/packages/elm/http/latest)
- Исходный код пакета `Base64` Elm: [package.elm-lang.org/packages/truqu/elm-base64/latest](https://package.elm-lang.org/packages/truqu/elm-base64/latest)
- RFC 7617, Схема базовой HTTP-аутентификации 'Basic': [tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
