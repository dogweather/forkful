---
title:                "Читання текстового файлу"
html_title:           "Clojure: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому
Text files є одним з найбільш поширених форматів для зберігання даних. Читання текстового файлу може бути корисним для використання даних в програмі або для отримання інформації для подальшого аналізу. Якщо ви вчите Clojure або працюєте з програмними данними, знання про читання текстових файлів буде корисним для вашої роботи.

## Як
Використовуючи вбудовану функцію `slurp`, ми можемо просто прочитати вміст текстового файлу в рядок.

```Clojure
(def data (slurp "file.txt"))
```

Цей рядок містить всю інформацію з нашого текстового файлу, і тепер ми можемо використовувати функції Clojure для обробки даних, які нам потрібні. Наприклад, ми можемо розділити рядок на список слів, використовуючи функцію `split` і поділити його за допомогою пробілів.

```Clojure
(def words (split data #"\s+"))
```

Можна також використовувати функції Clojure для перебору списків і виконання певних дій з кожним елементом. Наприклад, ми можемо видалити зайві символи з кожного слова в списку, використовуючи функцію `replace`.

```Clojure
(def cleaned-words (map #(replace % #"[^a-zA-Z]" "") words))
```

Тепер, у нас є список чистих слів, готовий для подальшої обробки. Ми також можемо використовувати функції Clojure для запису даних у новий файл. Наприклад, ми можемо створити новий файл із списком чистих слів, розділені комами.

```Clojure
(spool "cleaned_file.txt" (interpose "," cleaned-words))
```

## Deep Dive
Під час читання текстового файлу, Clojure автоматично перетворює його в рядок, використовуючи кодування UTF-8. Це означає, що ми можемо працювати з рядками, щоб отримати інформацію з файла або змінити контент перед записом у новий файл.

Також, варто відмітити, що Clojure підтримує читання як локальних файлів, так і файлів у мережі, за допомогою вказання URL.

## See Also
[The Clojure Cookbook: Reading and Writing Files](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/05_io.asciidoc#reading-and-writing-files) \
[ClojureDocs: slurp](https://clojuredocs.org/clojure.core/slurp) \
[ClojureDocs: spool](https://clojuredocs.org/clojure.core/spool)