---
title:                "Отримання поточної дати"
html_title:           "Elm: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Цікавишся програмуванням та хочеш дізнатися, як отримувати поточну дату у своїй програмі? Elm має вбудовану функцію для цього, і ми розкажемо тобі як її використати!

## Як це зробити

Для початку створимо змінну `currentDate`, яка буде містити поточну дату:

```elm
import Time

currentDate : Time.Time
currentDate = Time.now
```

Тепер, якщо вивести значення змінної `currentDate`, ми отримаємо поточну дату у форматі `Time.Time`:

```elm
"2021-03-10T10:36:17.638Z"
```

Якщо хочеш вивести дату у звичайному форматі (наприклад, 10 березня 2021), можна використати функцію `Time.inUtc`:

```elm
import Time
import Time.Extra exposing (calendarDay)

currentDate : Time.Posix
currentDate = Time.now

formattedDate : String
formattedDate = currentTime |> Time.inUtc |> Time.Extra.calendarDay
```

Отримаємо:

```elm
"10 березня 2021"
```

## Глибокий занурення

Можливо, ти помітив, що у функції `Time.inUtc` ми використали `Time.Posix` замість `Time.Time`. Це тому, що `Time.inUtc` приймає `Time.Posix` та повертає `( Date, Time )` – пару із дати та часу.

Також важливо пам'ятати, що `Time.now` повертає час у форматі `Time.Posix`, який представляє собою кількість мілісекунд з 1 січня 1970 року. Тому, якщо ти хочеш отримати час у потрібному форматі, можливо знадобиться використати додаткові функції з модулю `Time.Extra`.

## Дивись також

- [Офіційна документація Elm про Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Стаття "Основи Elm: Як працює дата і час"](https://medium.com/@lvivkrasno/time-in-elm-cd1c97b70e6b)
- [Книга "Elm в дії" про обробку дат та часу](https://livebook.manning.com/book/elm-in-action/chapter-7/72)