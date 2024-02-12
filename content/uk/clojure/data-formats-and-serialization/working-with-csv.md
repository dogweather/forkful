---
title:                "Робота з CSV"
aliases:
- /uk/clojure/working-with-csv.md
date:                  2024-02-03T19:19:25.980743-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Робота з файлами CSV (Comma-Separated Values - значення, розділені комами) включає парсинг та генерацію текстових даних, структурованих у вигляді рядків та колонок, подібно до даних електронних таблиць. Цей процес є важливим для обміну даними між програмами, базами даних, а також для завдань трансформації даних, завдяки широкому прийняттю CSV як легкого, інтероперабельного формату.

## Як це зробити:

### Читання файлу CSV
Clojure стандартно не має вбудованого парсингу CSV, але ви можете використовувати бібліотеку `clojure.data.csv` для цієї мети. Спочатку додайте бібліотеку до залежностей вашого проекту.

У вашому `project.clj` додайте таку залежність:
```clojure
[clojure.data.csv "1.0.0"]
```
Для того, щоб прочитати файл CSV і вивести кожен рядок:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "шлях/до/вашогогофайлу.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Це виведе кожен рядок CSV як вектор Clojure.

### Запис в файл CSV
Для запису даних у файл CSV ви можете використовувати ту саму бібліотеку `clojure.data.csv`:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "шлях/до/outputfile.csv")]
    (csv/write-csv writer data)))
```
Це створить або перезапише `outputfile.csv`, заповнюючи його вказаними даними.

### Використання бібліотеки стороннього розробника: `clojure.data.csv`

Хоча `clojure.data.csv` можна вважати найпростішою бібліотекою для обробки CSV у Clojure, для складніших завдань, таких як обробка CSV зі спеціальними символами або нестандартними роздільниками, ви можете розглянути додаткові варіанти в екосистемі або навіть розглянути можливість Java інтероперації з бібліотеками, такими як Apache Commons CSV. Однак, для більшості стандартних завдань обробки CSV у Clojure, `clojure.data.csv` забезпечує простий та ефективний набір інструментів.
