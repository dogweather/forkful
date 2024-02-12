---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:43:58.293552-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Завантаження веб-сторінки - це процес отримання її вмісту через інтернет. Програмісти роблять це, щоб отримати дані, якими можна маніпулювати та використовувати в своїх програмах.

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
