---
date: 2024-01-20 17:43:58.293552-07:00
description: "How to: (\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) \u0412\
  \ Elm, \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0437\u0430\u0432\u0430\
  \u043D\u0442\u0430\u0436\u0438\u0442\u0438 \u0432\u0435\u0431-\u0441\u0442\u043E\
  \u0440\u0456\u043D\u043A\u0443 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0447\u0438 `Http` \u043C\u043E\u0434\u0443\u043B\u044C. \u041D\
  \u0438\u0436\u0447\u0435 \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u0435\
  \u043D\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u0437\u0430\u043F\
  \u0438\u0442\u0443 \u0434\u043E \u0441\u0435\u0440\u0432\u0435\u0440\u0430 \u0442\
  \u0430 \u043E\u0431\u0440\u043E\u0431\u043A\u0438\u2026"
lastmod: '2024-04-05T21:53:49.354885-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) \u0412 Elm, \u0432\
  \u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0437\u0430\u0432\u0430\u043D\u0442\
  \u0430\u0436\u0438\u0442\u0438 \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\
  \u043D\u043A\u0443 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u044E\u0447\u0438 `Http` \u043C\u043E\u0434\u0443\u043B\u044C."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

## How to: (Як зробити:)
В Elm, ви можете завантажити веб-сторінку використовуючи `Http` модуль. Нижче представлений приклад запиту до сервера та обробки відповіді.

```Elm
import Http
import Json.Decode as Decode

type Msg
    = GotText (Result Http.Error String)

getText : Cmd Msg
getText =
    Http.get
        { url = "https://example.com"
        , expect = Http.expectString GotText
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText (Ok text) ->
            ({ model | content = text }, Cmd.none)

        GotText (Err _) ->
            (model, Cmd.none)

-- initialize and subscriptions omitted for brevity

```

У прикладі вище код робить HTTP GET запит до `"https://example.com"` і очікує на текстову відповідь.

## Deep Dive (Поглиблений Аналіз)
Завантаження веб-сторінок у Elm розпочалося з релізом мови, який перекладення веб-запитів на більш високий рівень абстракції. Elm використовує `Http` модуль, який робить чисті HTTP запити без бокових ефектів, узгоджуючись з архітектурою Elm. 
Альтернативою є пряме використання JavaScript через порти (ports), але це здебільшого підходить для складних випадків, де Elm `Http` модуль не пропонує потрібного функціоналу. Модуль `Http` використовує `Task` для представлення асинхронних запитів які можуть виконатися паралельно та легко об'єднуватися.

## See Also (Дивись також)
- Елм модуль Http: [Http package](https://package.elm-lang.org/packages/elm/http/latest)
- Про обробку JSON у Елм: [JSON Decoding](https://guide.elm-lang.org/effects/json.html)
- Офіційний Elm Гід: [Official Elm Guide](https://guide.elm-lang.org/)
