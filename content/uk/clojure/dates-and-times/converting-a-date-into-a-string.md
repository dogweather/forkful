---
title:                "Перетворення дати в рядок"
aliases:
- /uk/clojure/converting-a-date-into-a-string/
date:                  2024-01-20T17:36:31.948415-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Перетворення дати в рядок – це процес, де ми змінюємо формат представлення дати з внутрішньої структури на текст, який легко зрозуміти людям. Програмісти роблять це для відображення дат у зручному форматі або для серіалізації перед передачею даних.

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
