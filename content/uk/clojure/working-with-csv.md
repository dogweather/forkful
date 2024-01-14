---
title:                "Clojure: Робота з CSV"
simple_title:         "Робота з CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Робота з CSV є незаперечно корисною навичкою для бізнесу та збереження даних. За допомогою Clojure можна швидко та ефективно обробляти дані в форматі CSV, що дозволяє зберігати та аналізувати великі обсяги інформації швидше та ефективніше ніж за допомогою інших інструментів.

## Як

```Clojure
;; Завантаження та читання файлу CSV
(ns project.core
    (:require [clojure.data.csv :as csv]))

(defn read-csv [file]
  (with-open [reader (clojure.java.io/reader file)]
    (doall
      (csv/read-csv reader))))

(def csv-data (read-csv "file.csv"))

;; Вивід перших п'яти рядків даних
(take 5 csv-data)

[["ID" "Name" "Age" "Sex"]
 ["1" "John" "25" "Male"]
 ["2" "Maria" "32" "Female"]
 ["3" "James" "41" "Male"]
 ["4" "Anna" "27" "Female"]]

;; Фільтрація даних за певною умовою
(->> csv-data 
     (filter #(= (nth % 3) "Female")))

[["2" "Maria" "32" "Female"]
 ["4" "Anna" "27" "Female"]]

;; Створення нового файлу CSV з результатами фільтрації
(with-open [writer (clojure.java.io/writer "filtered.csv")]
  (csv/write-csv writer (filter #(= (nth % 3) "Female") csv-data)))
```

## Глибоке дослідження

При роботі з CSV у Clojure є кілька кроків, які потрібно виконати для успішної обробки та аналізу даних. Для початку, потрібно завантажити та прочитати файл CSV за допомогою функції `read-csv`. Потім, можна використовувати функції зі списково-орієнтованої програми для фільтрації та маніпулювання даними за потребою. Нарешті, за допомогою `write-csv`, можна створити новий файл з обробленими даними.

## Дивіться також

* [Clojure Cookbook on CSV](https://jacekschae.github.io/clojure-cookbook/file-and-io/handling-comma-separated-value-csv-format.html)
* [Clojure CSV Library](https://github.com/clojure/data.csv)