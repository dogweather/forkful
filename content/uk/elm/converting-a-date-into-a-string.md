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

# Чому
Численні програми побудовані на базі дат, проте часом потрібно перетворити цю дату на зручну для відображення форму. Elm має потужний механізм для перетворення дат в різні формати, зокрема у рядкову форму, що робить його універсальним і зручним для використання.

# Як
```Elm
import Time exposing (Posix)

-- Приклад перетворення дати у рядкову форму "MM/DD/YYYY"
dateToString : Posix -> String
dateToString date =
    Time.format "%m/%d/%Y" date
```

```Elm
-- Вихід: "11/17/2021"
dateToString 1647699600
```

# Вглиб
Перетворення дати в рядковий формат відбувається за допомогою модуля Time та його функції `format`. Ця функція приймає два аргументи: формат, за яким потрібно відображати дату, та саму дату у форматі Posix. Формат має бути рядком і може складатися з різних специфікаторів, які визначають, яка частина дати буде відображатися, наприклад `%m` - номер місяця, `%d` - номер дня, `%Y` - рік. Також є можливість додавати розділювачі, наприклад `/` або `-`, між частинами дати. Повний перелік специфікаторів можна знайти у [документації Elm](https://package.elm-lang.org/packages/elm/time/latest/Time#format).

# Дивитися також
- [Офіційна документація Elm](https://elm-lang.org/docs)
- [Стаття "Введення в програмування на Elm"](https://dev.to/zachgrayio/getting-started-with-elm-2gp3)
- [Курс по Elm на сайті Codecademy](https://www.codecademy.com/learn/learnelm)