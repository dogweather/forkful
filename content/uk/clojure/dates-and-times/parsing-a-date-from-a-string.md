---
title:                "Розбір дати з рядка"
aliases:
- /uk/clojure/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:08.182286-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
Розбір дати з рядка в Clojure полягає у перетворенні текстових представлень дат і часу в більш зручну форму (наприклад, об'єкт DateTime Clojure). Цей процес є фундаментальним для обробки даних, ведення журналу або будь-якого застосування, що маніпулює тимчасовими даними, дозволяючи програмістам виконувати операції, порівняння або маніпуляції з датами ефективно.

## Як:
Оскільки Clojure є мовою JVM, ви можете безпосередньо використовувати бібліотеки Java для дати та часу. Почнемо з вбудованої взаємодії з Java, а потім розглянемо, як використовувати популярну сторонню бібліотеку clj-time для більш ідіоматичних рішень Clojure.

### Використання взаємодії з Java
Clojure може безпосередньо використовувати `java.time.LocalDate` Java для розбору дат з рядків:
```clojure
(require '[clojure.java.io :as io])

; Розбір дати використовуючи взаємодію з Java
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; Вивід: 2023-04-01
```

### Використання clj-time
Більш ідіоматичною бібліотекою Clojure для роботи з датами та часом є `clj-time`. Вона обгортає Joda-Time, всеосяжну бібліотеку для операцій з датою та часом. Спочатку вам потрібно додати `clj-time` до залежностей вашого проекту. Ось як ви можете розібрати рядок дати використовуючи `clj-time`:

```clojure
; Переконайтеся, що додали [clj-time "0.15.2"] до вашого project.clj в :dependencies

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; Визначте форматер
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; Вивід: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

Ці приклади демонструють базовий розбір дат. Обидва методи корисні, але `clj-time` може забезпечити більш специфічний для Clojure підхід з додатковими функціональностями для складних вимог.
