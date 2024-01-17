---
title:                "Написання текстового файлу"
html_title:           "Clojure: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що & Чому?
Запис текстового файлу - це процес створення файлу для зберігання текстової інформації. Програмісти часто використовують цю технологію для зберігання важливих даних, таких як налаштування програм або звіти про помилки.

## Як:
```Clojure
(with-open [out (clojure.java.io/writer "file.txt")] ; створюємо з'єднання з файлом
  (doseq [line ["Перший рядок" "Другий рядок" "Третій рядок"]]
    (.write out (str line "\n")))) ; додаємо рядки до файлу з роздільником
; у файлі з'явиться:
; Перший рядок
; Другий рядок
; Третій рядок
```

## Глибше:
Технологія запису текстових файлів є вельми розповсюдженою і доступною для багатьох мов програмування, включаючи Clojure. Існують також альтернативні методи для зберігання даних, такі як реляційні бази даних, але запис до текстових файлів є часто використовуваним і простим способом. У Clojure, для запису тексту до файлу, можна використовувати не тільки функцію (.write), а й більш потужні бібліотеки, такі як clojure.data.csv для роботи зі структурованими даними.

## Також дивіться:
Для отримання додаткової інформації про запис до текстових файлів у Clojure, дивіться наступні джерела:
- [Офіційна документація Clojure](https://clojure.org/)
- [Clojure for the Brave and True: Learn the Ultimate Language and Become a Better Programmer](http://www.braveclojure.com/writing/)
- [Туторіал по роботі з текстовими файлами в Clojure](https://github.com/clojure/clojure/blob/master/doc/cheatsheet.txt)