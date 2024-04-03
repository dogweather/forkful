---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:25.980743-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : #."
lastmod: '2024-03-13T22:44:48.691172-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

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
