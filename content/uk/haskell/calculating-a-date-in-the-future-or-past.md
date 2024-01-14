---
title:                "Haskell: Обчислення дати у майбутньому або минулому"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Чи коли-небудь ви хотіли дізнатися яка буде дата через певну кількість днів? А можливо, вам потрібно було обчислити дату з минулого для звіту чи прогнозу? У цій статті ми покажемо вам, як використовувати Haskell для роботи з датами в майбутньому та минулому.

## Як

Кодування дат в майбутньому та минулому за допомогою Haskell дуже просте. Ми можемо використовувати функцію `addDays` для додавання або віднімання днів від поточної дати.

```Haskell
-- Для обчислення дати через 5 днів
addDays 5 currentDate

-- Для обчислення дати з 3 днів назад
addDays (-3) currentDate
```

Результатом буде нова дата в форматі `Year-Month-Day`.

## Глибоке погруження

Використання функції `addDays` може бути корисним, якщо ви хочете швидко обчислити дату в майбутньому або минулому, але що, якщо ви хочете використовувати більш структуровану форму дати? У Haskell, є багато різних типів для роботи з датами, такі як `Day`, `UTCTime`, `LocalTime` та інші.

Наприклад, ми можемо використовувати параметризовану функцію `Day` для конвертації з рядка в об'єкт типу `Day`, що представляє дату.

```Haskell
-- Перетворення з рядка в об'єкт типу Day
readTime defaultTimeLocale "%Y-%m-%d" "2000-01-01" :: Day
```

Це дасть нам об'єкт типу `Day`, який ми можемо використовувати для обчислення дати в майбутньому або минулому.

## Дивись також

- [Функція `addDays` в стандартній бібліотеці Haskell](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html#v:addDays)
- [Робота з датами в Haskell](https://www.haskell.org/hoogle/?hoogle=day)
- [Конвертація дат в Haskell за допомогою `readTime`](https://stackoverflow.com/questions/52761599/how-to-convert-a-string-into-a-date-in-haskell)