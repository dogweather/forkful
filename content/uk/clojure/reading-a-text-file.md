---
title:                "Clojure: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

Чому: Перш за все, читання текстових файлів є важливою навичкою для програмістів будь-якої мови програмування. В цій статті ми розглянемо, як це зробити в мові Clojure.

Як робити: Спочатку, нам потрібно відкрити файл за допомогою функції "with-open" і передати його у функцію "reader". Потім ми можемо використати функцію "line-seq", щоб прочитати файл по рядках і повернути список. Наприклад:

```Clojure
(with-open [reader (reader "myfile.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
```

Це простий приклад, який виведе вміст файлу "myfile.txt" на екран.

Глибокий занурення: Важливо пам'ятати, що читання файлів може бути викликом, особливо коли ми маємо справу з більшими файлами. Тому рекомендується використовувати функцію "with-open" для автоматичного закриття файла після завершення операцій з ним. Також ми можемо використовувати функцію "slurp", яка читає вміст файлу і повертає його як рядок.

Для більш детальної інформації про читання файлів у Clojure варто ознайомитися з офіційною документацією: https://clojuredocs.org/clojure.core/reader

Дивіться також: 

- https://www.braveclojure.com/files/
- https://www.lispcast.com/clojure-file-input-output
- https://www.tutorialspoint.com/clojure/clojure_file_io.htm

Переклад: Тимчасовий редактор, Airlines Manager