---
title:                "Обрахунок дати у майбутньому або минулому"
html_title:           "Elm: Обрахунок дати у майбутньому або минулому"
simple_title:         "Обрахунок дати у майбутньому або минулому"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Навіщо

У сучасному світі, де кожна мить має значення, важливо бути організованим і планувати свій час заздалегідь. Обчислення дати в майбутньому або минулому може бути корисним у випадках, коли потрібно з'ясувати, коли, наприклад, народився или помер хтось з близьких людей або коли потрібно вирішити, коли погода буде сприятливою для відпустки.

## Як це зробити

```Elm
import Time exposing (posixToMillis, millisToPosix, fromIsoString, add)

-- Функція add дозволяє додавати або віднімати певну кількість часу з даної дати
-- за допомогою оператора |> (pipe)
-- Для обчислення дати 1 місяць в майбутньому:
fromIsoString "2021-01-01"
    |> add Time.month 1
    |> millisToPosix

-- Результат: 1612132800000 - час в мілісекундах з початку епохи UNIX

-- Якщо потрібно обчислити дату 2 місяці назад:
fromIsoString "2021-01-01"
    |> add Time.month -2
    |> millisToPosix
    
-- Результат: 1604188800000
```

## Глибоке погруження

При обчисленні дати в майбутньому або минулому, важливо враховувати часовий пояс користувача. Elm має унікальний тип даних `Posix` для роботи з часом та функції `fromIsoString` та `millisToPosix`, які допоможуть перетворити дату з рядка у форматі ISO до мілісекунд та навпаки. Крім того, функція `add` дозволяє додавати різні одиниці часу, такі як місяці, тижні та дні, до відповідної дати.

## Дивіться також

- [Документація Elm по модулю Time](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Стаття "Getting Started with Elm" на сайті MDN](https://developer.mozilla.org/en-US/docs/Learn/Tools_and_testing/Client-side_JavaScript_frameworks/Elm_getting_started)
- [Курс "Learn Elm" на сайті Codecademy](https://www.codecademy.com/learn/learn-elm)