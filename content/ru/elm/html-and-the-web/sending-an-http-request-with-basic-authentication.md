---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
aliases:
- /ru/elm/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-29T00:02:41.850293-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса с базовой аутентификацией включает в себя добавление учетных данных (имя пользователя и пароль) в заголовки запроса для доступа к защищенным ресурсам. Программисты используют ее для простой аутентификации на HTTP API, когда не требуется сложная система.

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
