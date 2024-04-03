---
date: 2024-01-20 17:36:31.948415-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: Clojure \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454 `java.util` \u0431\u0456\
  \u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443 \u0434\u043B\u044F \u0440\u043E\
  \u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438. \u041E\u0441\
  \u044C \u044F\u043A \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0438\u0442\
  \u0438 \u0434\u0430\u0442\u0443 \u0432 \u0440\u044F\u0434\u043E\u043A."
lastmod: '2024-03-13T22:44:48.672804-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0454 `java.util` \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443\
  \ \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\
  \u0430\u043C\u0438."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## Як робити:
Clojure використовує `java.util` бібліотеку для роботи з датами. Ось як перетворити дату в рядок:

```Clojure
(import '[java.text SimpleDateFormat]
        '[java.util Date])

(defn format-date [date pattern]
  (let [formatter (SimpleDateFormat. pattern)]
    (.format formatter date)))

;; використання функції
(def my-date (Date.))

;; Приклад: перетворення дати на рядок у форматі "yyyy-MM-dd"
(println (format-date my-date "yyyy-MM-dd"))

;; виведення: "2023-03-15" (але буде відповідати поточній даті)
```

## Поглиблений розгляд:
Перетворення дат у рядки – стандартне завдання, яке існує з часів ранніх мов програмування. В Clojure, як і в багатьох мовах JVM, для цього часто використовують Java бібліотеки. Важливо обирати правильний формат дати, щоб уникнути плутанини з регіональними стандартами. Альтернативою `java.util.Date` є `java.time` (з Java 8), який надає більшу гнучкість і вирішує проблеми старіших класів.

Альтернативні бібліотеки, наприклад, clj-time, базуються на Joda-Time і надають ідіоматичне API для Clojure. Вибір методу залежить від потреб програми та особистих переваг розробника.

## Додаткові джерела:
- [Clojure документація](https://clojure.org/)
- [Туторіал по Java `java.util.Date`](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Туторіал по Java `java.time`](https://docs.oracle.com/javase/tutorial/datetime/)
- [clj-time на GitHub](https://github.com/clj-time/clj-time)
