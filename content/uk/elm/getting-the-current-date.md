---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Отримання поточної дати - це процес визначення дати (й часу, якщо потрібно) зараз. Розробники використовують його для створення імен файлів, журналування, тайм-стемпів, синхронізації даних та ін.

## Як це зробити:

Перш ніж отримати поточну дату, нам потрібно встановити пакет `justinmimbs/date`.

```
elm install justinmimbs/date
```

Потім у вашому Elm файлі, імпортуйте `Date` і створіть `Model` з полем `dateNow : Maybe Date.Date`.

```Elm
import Date exposing (Date)
import Task
import Time exposing (Posix, Zone, utc)

type alias Model =
  { dateNow : Maybe Date }

init : Flags -> ( Model, Cmd Msg )
init flags =
   ( { dateNow = Nothing }
   , Task.perform NewDate Date.now 
   )
...

type Msg
  = NewDate Date

...

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewDate newDate ->
      { model | dateNow = Just newDate } ! []
```

Тепер, коли ви запустите програму, вона надішле команду, щоб отримати поточну дату. Ця дата збережеться в `model.dateNow`.

## Поглиблений огляд:

1. Історичний контекст: Elm був створений для спрощення фронт-енд коду та покращення надійності веб-додатків через більш строгу структуру. Раніше Elm не мав вбудованого рішення для отримання дати та часу, тому була створена бібліотека `justinmimbs/date`.
2. Альтернативи: Іншою опцією є використання зовнішніх JS-бібліотек через порти. Однак цього варто уникати, оскільки Elm намагається мінімізувати залежності від JS.
3. Деталі реалізації: `Date.now` використовує Time API у Elm для отримання поточного часу в мілісекундах від початку епохи Unix (1970-01-01 00:00:00 UTC) і конвертує його в тип `Date`.

## Дивіться також:

- [Документація Elm за Date](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Бібліотека justinmimbs/date](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- [Elm Time API](https://package.elm-lang.org/packages/elm/time/latest/)