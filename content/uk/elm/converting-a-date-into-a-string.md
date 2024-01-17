---
title:                "Перетворення дати в рядок"
html_title:           "Elm: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Перетворення дати в рядок - це процес перетворення дати в зручний для зберігання та передачі формат. Програмісти часто використовують цей процес для роботи зі змінною типу дата та передачі її у форматі, зрозумілому для комп'ютера.

## Як це зробити:
```Elm
import Date exposing (toIsoString)

date = Date.fromCalendarDate 2019 9 17

toIsoString date -- поверне "2019-09-17"
```

## Глибоке погруження:
Цей метод перетворення дати в рядок використовується з 1970-ї роки, коли стандартизований формат був розроблений для збереження та передачі дат на комп'ютерах. Є також інші методи перетворення дати, такі як використання часової зони або Unicode-списку днів тижня. У Elm, можна використовувати бібліотеку DateTime для більшістю цих методів.

## Подивіться також:
- [Офіційна документація Elm з перетворення дати в рядок](https://guide.elm-lang.org/interop/dates_times.html)
- [Використання глобальної часової зони в Elm](https://package.elm-lang.org/packages/elm/time/latest/Time#utc)
- [Бібліотека DateTime для повного переліку методів перетворення дати в рядок](https://package.elm-lang.org/packages/justinmimbs/date-time/latest/)