---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення дати в рядок - це процес, коли числове представлення дати або часу перетворюється на текстовий формат. Програмісти роблять це для кращого відображення дати або часу в інтерфейсі користувача або для зручного форматування даних.

## Як це зробити:

Elm надає вам можливість використовувати вбудовані функції, такі як `toString` для перетворення дати в рядок. Давайте подивимося, як це працює.

```Elm
import Time exposing (Posix)
import Time.Extra exposing (toHumanReadable)

view : Posix -> Html.Html Msg
view posix =
    text (toHumanReadable posix)

-- Використання
view (Time.posix 1638739200000)
-- Виведення: "2021-12-05T12:00:00.000Z"
```

## Пірнання глибше

Насправді, Elm не мав вбудованого механізму для обробки дати і часу до версії 0.19.1, а також не мав вбудованого типу DateTime. Проте, додавання підтримки POSIX часу зробило роботу з датою і часом значно простіше. 

Альтернативами вбудованим функціям в Elm можуть бути використання JS інтероперабельності через порти або кодування власних функцій для перетворення дати. 

Важливо зрозуміти, що функція `toString` в Elm працює з POSIX часом, що представляє час як кількість мілісекунд, що минули з 1 січня 1970 року (UTC).

## Дивись також

1. [Time in Elm](https://package.elm-lang.org/packages/elm/time/latest/Time) - документація по роботі з часом в Elm.
2. [Elm Date Libraries](https://community.elm-lang.org/t/date-libraries-in-elm/320/6) - обговорення різних бібліотек для роботи з датою в Elm.
3. [Formatting Dates in Elm](https://korban.net/posts/elm/2019-10-15-formatting-dates-in-elm/) - стаття про форматування дат в Elm.