---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що та Чому?
Запис текстових файлів – це збереження даних у формі тексту в файлі. Програмісти роблять це для збереження інформації, налаштувань, логів, або для обміну даними між програмами.

## Як робити:
```clojure
;; Підключення необхідних просторів імен
(require '[clojure.java.io :as io])

;; Запис у текстовий файл
(with-open [wrtr (io/writer "path/to/yourfile.txt")]
  (.write wrtr "Привіт, це приклад запису у файл!"))

;; Читання та перевірка вмісту записаного файлу
(slurp "path/to/yourfile.txt")
;; => "Привіт, це приклад запису у файл!"
```

## Поглиблений огляд
В історії, запис даних у файл був одним із основних способів збереження інформації. Альтернативи? Використання баз даних, віддалених хмарних сервісів. Як це робить Clojure? Clojure використовує Java I/O – міцну та перевірену частину JDK для роботи з файлами. Використання `with-open` гарантує закриття файлу, навіть якщо виникає помилка.

## Дивіться також
- Clojure Documentation: https://clojure.org/guides/deps_and_cli
- "Clojure for the Brave and True" by Daniel Higginbotham: https://www.braveclojure.com/io/
- Java I/O Docs: https://docs.oracle.com/javase/tutorial/essential/io/
