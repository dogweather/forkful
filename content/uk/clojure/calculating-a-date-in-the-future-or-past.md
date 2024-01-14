---
title:                "Clojure: Розрахунок дати в майбутньому або минулому"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Розрахунок дати в майбутньому або минулому може бути корисним для створення напоминальних сервісів, додавання термінів дії до подій або виведення списку подій за певний період часу.

## Як це зробити

```Clojure
;; імпортуємо бібліотеку `clj-time`
(require '[clj-time.core :as time])

;; розрахунок дати за допомогою функції `plus`
(time/plus (time/today) (time/days 5))

;; вивід: #inst "2021-05-15T00:00:00.000-00:00" (дата +5 днів)

;; розрахунок дати в минулому за допомогою функції `minus`
(time/minus (time/today) (time/weeks 2))

;; вивід: #inst "2021-04-24T00:00:00.000-00:00" (дата -2 тижні)

```

## Глибше в деталі

Бібліотека `clj-time` надає багато функцій для роботи з датами, таких як `day-of-week`, `month-of-year`, `year`, та інших, які можуть бути корисними для більш точного розрахункці дати. Також, вона має вбудовані функції для перетворення дати в різноманітні формати, такі як `format` та `parse`.

## Дивіться також

- [Офіційна документація бібліотеки `clj-time`](https://clj-time.github.io/clj-time/)
- [Розрахунок дат з `java.time` у Clojure](https://blog.circleci.com/clojure-java-time/)
- [Бібліотека `tick`](https://github.com/johnmn3/tick), представна альтернатива `clj-time` для роботи з датами.