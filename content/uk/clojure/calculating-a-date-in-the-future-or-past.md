---
title:                "Обчислення дати у майбутньому або минулому"
html_title:           "Clojure: Обчислення дати у майбутньому або минулому"
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Порахувати дату в майбутньому або в минулому може знадобитися у випадку, коли потрібно планувати події або розслідувати минулі події.

## Як

Для обчислення дати в майбутньому або в минулому використовуються функції з модуля `clojure.java-time`, наприклад `plus` і `minus`.

```Clojure
(require '[java-time :as jt])

(def now (jt/now))
(jt/plus now (jt/period (days 5))) ; дата, яка буде через 5 днів
; => #object[java.time.LocalDateTime 0x7b037c35 "2021-05-12T15:32:17.765071"]

(jt/minus now (jt/period (weeks 2))) ; дата, яка була 2 тижні тому
; => #object[java.time.LocalDateTime 0xf5a4fc6 "2021-04-14T15:32:17.765071"]
```

## Глибокий занурення

У модулі `clojure.java-time` є багато функцій для роботи з датами, такі як обчислення різниці між датами, перетворення до різних форматів і багато іншого. Можна ознайомитися з документацією для більш детальної інформації.

## Дивіться також

- [Офіційна документація clojure.java-time](https://clojure.github.io/java-time/)
- [Стаття про роботу з датами у Clojure](https://purelyfunctional.tv/guide/clojure-date-time/)