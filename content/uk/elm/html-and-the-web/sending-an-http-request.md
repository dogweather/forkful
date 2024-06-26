---
date: 2024-01-20 18:00:05.261278-07:00
description: "\u0429\u043E \u0439 \u041D\u0430\u0432\u0456\u0449\u043E? \u0412 Elm,\
  \ \u0432\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F HTTP\
  \ \u0437\u0430\u043F\u0438\u0442\u0443 \u0434\u043E\u0437\u0432\u043E\u043B\u044F\
  \u0454 \u0442\u0432\u043E\u0457\u0439 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456 \u043A\u043E\u043C\u0443\u043D\u0456\u043A\u0443\u0432\u0430\u0442\u0438\
  \ \u0437 \u0432\u0435\u0431-\u0441\u0435\u0440\u0432\u0435\u0440\u0430\u043C\u0438\
  . \u0426\u0435 \u043A\u0440\u0438\u0442\u0438\u0447\u043D\u043E \u0434\u043B\u044F\
  \ \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0434\
  \u0430\u043D\u0438\u0445, \u0430\u0432\u0442\u043E\u0440\u0438\u0437\u0430\u0446\
  \u0456\u0457\u2026"
lastmod: '2024-03-13T22:44:49.145153-06:00'
model: gpt-4-1106-preview
summary: "\u0412 Elm, \u0432\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\
  \u043D\u044F HTTP \u0437\u0430\u043F\u0438\u0442\u0443 \u0434\u043E\u0437\u0432\u043E\
  \u043B\u044F\u0454 \u0442\u0432\u043E\u0457\u0439 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456 \u043A\u043E\u043C\u0443\u043D\u0456\u043A\u0443\u0432\u0430\u0442\
  \u0438 \u0437 \u0432\u0435\u0431-\u0441\u0435\u0440\u0432\u0435\u0440\u0430\u043C\
  \u0438."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## Що й Навіщо?
В Elm, відправлення HTTP запиту дозволяє твоїй програмі комунікувати з веб-серверами. Це критично для завантаження даних, авторизації користувачів, відправлення форм.

## How to:


## Як це зробити:
```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)

getUser : Cmd Msg
getUser =
    Http.get
        { url = "https://example.com/user"
        , expect = Http.expectJson GotUser userDecoder
        }

type Msg
    = GotUser (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotUser (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        GotUser (Err _) ->
            (model, Cmd.none)
```

Запит `getUser` видасть повідомлення `GotUser` з результатом.

## Deep Dive


## Поглиблений Занурення
Elm's HTTP бібліотека є надійним та тип-безпечним способом взаємодії з API. У 2016 році, Elm переосмислив обробку побічних ефектів з "Elm Architecture", де HTTP виклики стали командами (`Cmd`). Альтернативою Elm є JavaScript з axios чи fetch, але без гарантій безпеки типів. В Elm, ти описуєш очікуваний вигляд даних за допомогою декодерів, забезпечуючи додаткову впевненість в правильності даних.

## See Also


## Дивіться Також
- Офіційна документація по HTTP в Elm: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Роз'яснення "Elm Architecture": [https://guide.elm-lang.org/architecture/](https://guide.elm-lang.org/architecture/)
- Про JSON декодування: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
