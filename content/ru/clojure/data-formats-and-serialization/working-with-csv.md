---
title:                "Работа с CSV"
aliases: - /ru/clojure/working-with-csv.md
date:                  2024-01-29T00:04:27.636801-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Работа с CSV (значения, разделённые запятыми) означает обработку табличных данных, сохранённых в формате простого текста. Программисты делают это, потому что обработка CSV - это общая потребность для обмена данными и быстрого хранения, поскольку это формат читаемый, простой и поддерживаемый множеством инструментов.

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
