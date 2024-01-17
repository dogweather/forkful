---
title:                "Робота з csv"
html_title:           "Clojure: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & чому?
CSV - це формат для представлення табличних даних в текстовому вигляді, де кожен рядок відповідає одному рядку таблиці, а значення розділені комами. Програмісти часто працюють з CSV для обробки, аналізу та імпорту даних з різних джерел.

## Як це робити:
```Clojure
(ns my-project.core
  (:require [clojure.data.csv :as csv]))

(defn read-csv [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (doall (csv/read-csv rdr))))

(defn write-csv [file data]
  (with-open [wrtr (clojure.java.io/writer file)]
    (csv/write-csv wrtr data)))
```

## Глибинне занурення:
CSV був створений в 1972 році і використовується до сьогодні. Існують інші формати для подібних завдань, наприклад, JSON та XML, але CSV є простим і легко у використанні. У Clojure існує багато бібліотек для роботи з CSV, але найбільш популярною є clojure.data.csv. Ця бібліотека надає функції для читання та запису CSV файлів, а також можливість вказати власний роздільник і знак кінця рядка.

## Дивись також:
- [ClojureDocs: clojure.data.csv](https://clojuredocs.org/clojure.data.csv)
- [Official Clojure CSV Library Documentation](https://clojure.github.io/data.csv/)
- [A Comprehensive Guide to Working with CSV files in Clojure](https://purelyfunctional.tv/guide/clojure-csv/)