---
title:                "Обчислення дати у майбутньому або минулому"
aliases:
- uk/clojure/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:06.725550-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Визначення дати у майбутньому чи минулому — це процес обчислення дат на основі поданих термінів. Програмісти виконують цю задачу для розрахунку термінів, планування завдань, чи відслідковування часу подій.

## Як це зробити:
Clojure включає бібліотеку `clj-time`, яку можна використовувати для маніпуляцій із датами. Для прикладу додамо 5 днів до поточної дати:

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])

;; Створення поточної дати
(def today (t/now))

;; Додавання 5 днів до поточної дати
(def future-date (t/plus today (t/days 5)))

;; Виведення в форматі yyyy-MM-dd
(println (c/to-string future-date))
```
А тепер віднімемо 10 днів від конкретної дати:

```Clojure
;; Визначення конкретної дати
(def some-date (t/date-time 2023 4 1))

;; Віднімання 10 днів від визначеної дати
(def past-date (t/minus some-date (t/days 10)))

;; Виведення в форматі yyyy-MM-dd
(println (c/to-string past-date))
```

## Поглиблено:
У минулому програмісти використовували стандартні Java бібліотеки для роботи з датами. Втім, `clj-time`, бібліотека заснована на Joda-Time, зробила маніпуляцію з датами в Clojure легшою та інтуітивно зрозумілою. Іншим вибором є новіший Java Time API (JSR-310), який також доступний у Clojure через Java інтероперабельність, але `clj-time` часто вибирають за зручний Clojure-інтерфейс. При обчисленні дат важливо враховувати часові зони та літній час, щоб уникнути помилок у розрахунках.

## Див. також:
- Більше про бібліотеку clj-time: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Посібник з Joda-Time: [http://www.joda.org/joda-time/quickstart.html](http://www.joda.org/joda-time/quickstart.html)
