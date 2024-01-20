---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати означає отримання миттєвого часового відбитка. Це широко використовується для реєстрації подій, аудиту, журналювання чи просто задання мітки часу.

## Як це зробити:
Приклади коду та результати виконання в блоці закодованого коду ```Clojure```.

```Clojure
(ns clj-time.core)

;; Потрібно завантажити бібліотеку
(require 'clj-time.core)

;; Отримати поточну дату
(def current-date (clj-time.core/now))

;; Друк поточної дати
(println current-date)
```

Приклад виводу:
```Clojure
#object[org.joda.time.DateTime 2022-03-08T12:34:56.789Z]
```

## Занурення у деталі
розробка Clojure розпочалася в 1957 році, з основною метою усунення деяких обмежень Java.
Clojure надає ряд підхідних функцій для роботи з датами та часом через бібліотеку Clj-time, яка базується на Joda-Time.

Альтернативно, ви можете використати Java Interop для використання Java API для дат і часу. Однак помноження Clojure з Java може бути трохи громіздким, тому Clj-time зазвичай є наданою вибором.

Щодо деталей реалізації, сама бібліотека Clj-time працює як обгортка навколо Joda-Time, надаючи більш дружній до Clojure інтерфейс для роботи з датами та часом.

## Дивіться також
1. [Офіційна документація Clojure](https://clojure.org/)
2. [Керівництво по бібліотеці Clj-time](https://github.com/clj-time/clj-time)
3. [Java API для Data Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)