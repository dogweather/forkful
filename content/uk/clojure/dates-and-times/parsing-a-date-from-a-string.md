---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:08.182286-07:00
description: "\u042F\u043A: \u041E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 Clojure\
  \ \u0454 \u043C\u043E\u0432\u043E\u044E JVM, \u0432\u0438 \u043C\u043E\u0436\u0435\
  \u0442\u0435 \u0431\u0435\u0437\u043F\u043E\u0441\u0435\u0440\u0435\u0434\u043D\u044C\
  \u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\
  \u0442\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438 Java \u0434\
  \u043B\u044F \u0434\u0430\u0442\u0438 \u0442\u0430 \u0447\u0430\u0441\u0443. \u041F\
  \u043E\u0447\u043D\u0435\u043C\u043E \u0437 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u0457\
  \ \u0437 Java, \u0430 \u043F\u043E\u0442\u0456\u043C\u2026"
lastmod: '2024-03-13T22:44:48.669213-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 Clojure \u0454 \u043C\u043E\
  \u0432\u043E\u044E JVM, \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0431\
  \u0435\u0437\u043F\u043E\u0441\u0435\u0440\u0435\u0434\u043D\u044C\u043E \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438 Java \u0434\u043B\u044F \u0434\
  \u0430\u0442\u0438 \u0442\u0430 \u0447\u0430\u0441\u0443."
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
weight: 30
---

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
