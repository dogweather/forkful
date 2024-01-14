---
title:                "Haskell: Отримання поточної дати"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

У цій статті ми розглянемо, як отримати поточну дату за допомогою мови програмування Haskell. Отримання поточної дати є важливим завданням для багатьох проектів, які пов'язані з обробкою даних, відслідковуванням часу тощо.

## Як це зробити

Одним зі способів отримати поточну дату є використання функції `getCurrentTime` з модуля `Data.Time`. Для цього потрібно спочатку імпортувати цей модуль, після чого можна викликати функцію `getCurrentTime` і зберегти її результат у змінну.

```Haskell
import Data.Time

currentDate <- getCurrentTime
```

Наступним кроком буде форматування дати за допомогою функції `formatTime`. Для цього потрібно вказати бажаний формат дати та передати отриману змінну з поточною датою.

```Haskell
currentDateFormatted <- formatTime defaultTimeLocale "%d/%m/%Y" currentDate
```

Після цього `currentDateFormatted` буде містити поточну дату у вказаному форматі. Наприклад, результат може бути `10/11/2021`.

## Глибоке поглиблення

Якщо ви хочете дізнатися більше про роботу з датами у Haskell, можна ознайомитися з документацією модуля `Data.Time`. Там ви знайдете інформацію про різні функції та формати дат, які можна використовувати у своїх проектах.

## Дивіться також

- [Документація по модулю Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Приклади використання функцій для отримання поточної дати](https://gist.github.com/nikita-skobov/6f716ec97a3b0a121f5f3c4d75b3225e)