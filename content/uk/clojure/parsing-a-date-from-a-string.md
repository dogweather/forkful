---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Розпізнавання дати з рядка - це процес, під час якого ми змінюємо текстову інформацію про дату на машинозрозумілий формат. Програмісти роблять це, щоб уникнути помилок вводу даних, полегшити обробку дати та забезпечити безпеку даних.

## Як це зробити:

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])

(defn parse-date [date-string]
  (f/parse (f/formatters :date-time-no-ms) date-string))

;; приклад використання
(println (parse-date "2021-08-29T14:30:00"))
```

Виконуючи цей код, ви отримаєте результат:
```Clojure
2021-08-29T14:30:00.000Z
```

## Занурення у тему

* *Історичний контекст*: Розпізнавання дати з рядка було важливою річчю для програмування з самого початку, починаючи від ранніх середовищ розробки.
* *Альтернативи*: Є інші методи розпізнавання дати в Clojure, включаючи використання Java Interop для виклику Java's SimpleDateFormat. Однак, за допомогою clj-time, все стає набагато простішим.
* *Деталі реалізації*: При розпізнаванні дати з рядка important є розуміння, що не всі формати рядків дати відповідають стандарту ISO 8601. clj-time використовує Joda-Time бібліотеку під капотом для обробки дати та часу.

## Див. також

* Офіційна документація clj-time: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time) 
* Підручник Clojure: [https://www.braveclojure.com/](https://www.braveclojure.com/) 

Парсинг дати - це одне з основних завдань, які ви виконуєте при роботі з даними у вашій додатках. Поважний виклик цього => підводити під машинний розуміння. Навчіться цьому наскрізно.