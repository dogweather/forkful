---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Працювати з CSV — це значить маніпулювати даними у форматі, де значення відокремлені комами. Програмісти це роблять для обробки та аналізу великих масивів даних, зручного експорту та імпорту між різними програмами.

## Як це робити:
```clojure
;; Підключаємо бібліотеку
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

;; Читання CSV файлу
(with-open [reader (io/reader "data.csv")]
  (doall (csv/read-csv reader)))

;; Запис у CSV файл
(let [data [["name" "age" "city"]
            ["Alice" "30" "Kyiv"]
            ["Bob" "35" "Lviv"]]]
  (with-open [writer (io/writer "output.csv")]
    (csv/write-csv writer data)))
```

## Поглиблений розгляд
CSV, або Comma-Separated Values, з'явився ще у 1970-их для збереження табличних даних. Є альтернативи як JSON, XML, але CSV досі популярний за простоту та широку підтримку. В Clojure, обробка CSV вимагає зазвичай зовнішньої бібліотеки, такої як `clojure.data.csv`, яка використовує lazy sequence для ефективності.

## Додатково:
- Clojure документація: https://clojure.org/
- Бібліотека `clojure.data.csv`: https://github.com/clojure/data.csv
- Робота з IO у Clojure: https://clojure.github.io/clojure/clojure.java.io-api.html
