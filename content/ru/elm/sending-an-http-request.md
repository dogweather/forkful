---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:02:44.208305-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

В Elm отправка HTTP-запроса — это способ, которым ваше приложение общается с другими веб-сервисами для обмена данными. Программисты делают это для получения или отправки информации на серверы, обеспечивая динамику приложений, такую как учетные записи пользователей, счета или обновления новостей.

## Как это сделать:

Хорошо, время кода. Elm отправляет HTTP-запросы, используя модуль `Http`. Вот быстрый пример для получения некоторого JSON:

```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , username : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)

fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = "https://api.example.com/user/1"
        , decoder = userDecoder
        }
        |> Http.send UserFetched

type Msg
    = UserFetched (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserFetched (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        UserFetched (Err _) ->
            (model, Cmd.none)
```

Пример вывода, когда `UserFetched` является `Ok user`:

```Elm
{ id = 1, username = "ElmerFudd" }
```

## Глубокое погружение

Отправка HTTP-запросов не нова; это было основой веб-коммуникаций с 90-х годов. Elm упаковывает сложность в дружественный модуль `Http`, сосредотачиваясь на безопасности и простоте. В отличие от ранних времен, Elm абстрагирует запутанные части, такие как XMLHttprequest и разбор JSON. Альтернативы, такие как использование Fetch API или XMLHttpRequest напрямую в JavaScript возможны с портами, но встроенный в Elm способ поддерживает безопасность типов и чистоту вашего кода. Он обрабатывает побочные эффекты через его мощную архитектуру, не снижая надежности вашего приложения.

## Смотрите также

Для более подробных объяснений и устранения неполадок, ознакомьтесь с этими ресурсами:

- Документация пакета Elm для HTTP: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Декодирование JSON в Elm: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- Руководство Elm по HTTP-запросам: [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
- Elm Discuss для понимания сообщества: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
