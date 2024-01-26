---
title:                "Порівняння двох дат"
date:                  2024-01-20T17:33:47.893774-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?
Порівняння двох дат — це процес визначення їх відносного порядку в часі. Програмісти роблять це, щоб організовувати події, валідувати періоди часу та управляти потоками даних, що залежать від часу.

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
