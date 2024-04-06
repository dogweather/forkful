---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:44.208305-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0425\u043E\u0440\u043E\u0448\u043E, \u0432\u0440\u0435\u043C\u044F\
  \ \u043A\u043E\u0434\u0430. Elm \u043E\u0442\u043F\u0440\u0430\u0432\u043B\u044F\
  \u0435\u0442 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u044B, \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u0443\u044F \u043C\u043E\u0434\u0443\u043B\u044C `Http`.\
  \ \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u043F\u0440\u0438\
  \u043C\u0435\u0440 \u0434\u043B\u044F \u043F\u043E\u043B\u0443\u0447\u0435\u043D\
  \u0438\u044F \u043D\u0435\u043A\u043E\u0442\u043E\u0440\u043E\u0433\u043E JSON."
lastmod: '2024-04-05T21:53:45.424419-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

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
