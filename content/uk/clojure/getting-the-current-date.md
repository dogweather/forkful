---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:13:51.661872-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?
Потрібно знати поточну дату, щоб відзначати час відправлення повідомлень, реєстрацію подій, або просто відображати її у вашій програмі. Програмісти часто використовують це для логування, таймштампів та інших функціональностей, де час є важливим.

## Як це зробити:
Отже, отримаємо поточну дату в Clojure. Для цього ми використаємо Java interop, оскільки Clojure працює на JVM.

```clojure
;; Підключення необхідного простору імен
(require '[clj-time.core :as t])

;; Отримання поточної дати і часу за допомогою clj-time
(defn get-current-date-time []
  (t/now))

;; Виклик функції та виведення результату
(println (get-current-date-time))
```

Вивід буде схожим на це:

```
"2023-03-15T12:34:56.789Z"
```

## Поглиблений розгляд:
Clojure була створена Річем Хікі (Rich Hickey) та почала набирати популярності приблизно у 2007 році. Вона використовує JVM і може використовувати Java бібліотеки, такі як `java.util.Date` або Joda-Time (через `clj-time` обгортку). Для отримання дати та часу в Clojure існують альтернативи як `java.time` в Java 8, але `clj-time` є більш "ідіоматичним" і зручним у використанні.

## Дивіться також:

- [clj-time GitHub репозиторій](https://github.com/clj-time/clj-time)

- [ClojureDocs - експериментальний сайт в допомогу плануванню](https://clojuredocs.org/)

- [Clojure офіційний сайт](https://clojure.org/)

- [Інструкція по java.time для Java 8](https://docs.oracle.com/javase/tutorial/datetime/)

Зверніть увагу, що наведені інструменти можуть змінюватися у майбутньому, тому завжди перевіряйте останні новини і рекомендації щодо роботи з датами і часом в Clojure.
