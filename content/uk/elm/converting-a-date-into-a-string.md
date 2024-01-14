---
title:                "Elm: Зміна дати у рядок"
simple_title:         "Зміна дати у рядок"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Чому

Ви можете подумати, що перетворення дати в рядок є незначною частиною програмування, але насправді це може бути важливим елементом у вашому проекті. Наприклад, ви можете використовувати рядок дати для збереження часу створення або оновлення об'єктів в базі даних або для відображення дати на екрані для користувача.

# Як

```Elm
import Date
import Time

-- Задаємо поточну дату та час
let now = Time.millisToPosix 1581817600000

-- Перетворюємо дату в рядок з заданим форматом
let formattedDate = Date.format "dd.MM.yyyy" now

-- Виводимо результат
formattedDate
```

Результат: "16.02.2020"

# Глибинне дослідження

Конвертація дати в рядок може бути складною задачею через різні формати дати та наявність часових зон. В Ельм є багато корисних функцій, які допомагають вам зробити це в простіший спосіб. Наприклад, можна використовувати типи даних, такі як `Date`, `Time` та `Posix` для представлення дати та часу в програмі.

# Дивіться також

- Офіційна документація Ельм по конвертації дати: https://guide.elm-lang.org/dates_and_times#formatting-dates
- Стаття "Робота з часом та датою в Ельм": https://betterprogramming.pub/working-with-time-and-date-in-elm-a8c598bdf55f
- Пакет "date-format": https://package.elm-lang.org/packages/elm/time/latest/Time-Format