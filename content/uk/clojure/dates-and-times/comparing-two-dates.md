---
date: 2024-01-20 17:33:47.893774-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Clojure \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0456\
  \ \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0434\u043B\u044F \u0440\u043E\u0431\
  \u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438, \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 java.util.Date,\
  \ \u0430\u043B\u0435 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437\
  \ java.time.LocalDate \u0431\u0443\u0434\u0435 \u043A\u0440\u0430\u0449\u0435 \u0447\
  \u0435\u0440\u0435\u0437\u2026"
lastmod: '2024-03-13T22:44:48.674464-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\
  \u0456 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u0434\u043B\u044F \u0440\u043E\
  \u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438, \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 java.util.Date,\
  \ \u0430\u043B\u0435 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437\
  \ java.time.LocalDate \u0431\u0443\u0434\u0435 \u043A\u0440\u0430\u0449\u0435 \u0447\
  \u0435\u0440\u0435\u0437 \u0456\u043C\u0443\u0442\u0430\u0431\u0435\u043B\u044C\u043D\
  \u0456\u0441\u0442\u044C."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Як це зробити:
Clojure має вбудовані функції для роботи з датами, використовуючи java.util.Date, але працювати з java.time.LocalDate буде краще через імутабельність.

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])

;; Припустимо, маємо дві дати в рядках, перетворимо їх в LocalDate:
(def date-str-1 "2023-03-15")
(def date-str-2 "2023-03-20")

(def fmt (f/formatters :basic-date))
(def date-1 (t/local-date (f/parse fmt date-str-1)))
(def date-2 (t/local-date (f/parse fmt date-str-2)))

;; Порівнюємо дати:
(t/before? date-1 date-2) ;; -> true, оскільки date-1 є до date-2
(t/after? date-1 date-2) ;; -> false, оскільки date-1 не є після date-2
(t/equal? date-1 date-2) ;; -> false, оскільки date-1 не дорівнює date-2
```

## В глибину:
У Clojure для порівняння дат більш звично використовувати clj-time, який є обгорткою для Joda-Time до виходу Java 8. Після Java 8 рекомендується використовувати java.time.* API, через його імутабельність та краще API.

Існують альтернативи, наприклад, `cljc.java-time` бібліотека, яка намагається забезпечити однаковий досвід у Clojure і ClojureScript, опираючись на java.time and JS-Joda відповідно.

Робота із часовими зонами та літнім часом може спричинити непередбачувані результати, тому важливо правильно використовувати часові зони при порівнянні дат.

## Посилання:
- CLJ-Time GitHub репозиторій: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Бібліотека cljc.java-time: [https://github.com/henryw374/cljc.java-time](https://github.com/henryw374/cljc.java-time)
