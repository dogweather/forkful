---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:27.636801-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0437\
  \u0430\u043A\u0430\u0442\u0430\u0435\u043C \u0440\u0443\u043A\u0430\u0432\u0430\
  \ \u0438 \u0440\u0430\u0437\u0431\u0435\u0440\u0451\u043C \u0444\u0430\u0439\u043B\
  \ CSV \u0432 Clojure."
lastmod: '2024-03-13T22:44:44.390691-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0437\u0430\u043A\u0430\u0442\
  \u0430\u0435\u043C \u0440\u0443\u043A\u0430\u0432\u0430 \u0438 \u0440\u0430\u0437\
  \u0431\u0435\u0440\u0451\u043C \u0444\u0430\u0439\u043B CSV \u0432 Clojure."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как:
Давайте закатаем рукава и разберём файл CSV в Clojure.

```Clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "data.csv")]
  (let [data (csv/read-csv reader)]
    (doseq [row data]
      (println row))))
```

Пример вывода для CSV с "name,age" будет следующим:

```Clojure
["John" "30"]
["Jane" "25"]
["Doe" "40"]
```

Для записи данных в файл CSV:

```Clojure
(with-open [writer (io/writer "output.csv")]
  (csv/write-csv writer [["name" "age"]
                         ["John" "30"]
                         ["Jane" "25"]
                         ["Doe" "40"]]))
```

Это запишет указанные строки в `output.csv`.

## Глубокое погружение
Обработка CSV в Clojure довольно проста по сравнению с другими языками - без лишних сложностей. Исторически, простота CSV сделала его широко распространённым для обмена данными, опережая многие форматы данных. Альтернативы включают JSON, XML, или YAML, но CSV выигрывает там, где ключевыми являются простота или совместимость с электронными таблицами. Библиотека `clojure.data.csv` предоставляет основы для разбора и записи CSV, построенные на эффективных потоках ввода-вывода Java для хорошей производительности.

## Смотрите также
1. CSV библиотека Clojure: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
2. Подробнее о CSV: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
3. Для погружения в Clojure: [https://clojure.org/](https://clojure.org/)
