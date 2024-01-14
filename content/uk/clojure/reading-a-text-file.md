---
title:    "Clojure: Читання текстового файлу"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Робота з текстовими файлами є невідємною частиною розробки програм на Clojure. Це дозволяє зчитувати та обробляти дані з зовнішніх джерел, таких як CSV-файли, лог-файли та інші текстові формати. 

## Як

Один з способів зчитування текстових файлів - використання вбудованих функцій `slurp` та `line-seq`. Наприклад, якщо ми маємо файл `file.txt`, який містить наступний текст:

```
Hello
World
```

Тоді за допомогою функції `slurp` ми можемо зчитати весь вміст файлу у вигляді одного рядка: 

```Clojure
(slurp "file.txt") ; => "Hello\nWorld"
```

Також ми можемо використовувати функцію `line-seq` для зчитування файлу по рядках: 

```Clojure
(def lines (line-seq (slurp "file.txt")))
(first lines) ; => "Hello"
(second lines) ; => "World"
```

## Глибше

Для більшої гнучкості та контролю над зчитуванням текстових файлів, ми можемо використовувати бібліотеку `clojure.java.io`. З її допомогою, ми можемо відкривати файл, читати його по рядках та обробляти ці рядки за потреби. Наприклад: 

```Clojure
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "file.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
    
; => Hello
; => World
```

Для більш складних операцій, таких як розбиття рядка на слова чи робота з CSV-файлами, варто розглянути використання бібліотеки [clojure-csv](https://github.com/clojure-csv/clojure-csv).

## Дивись також

- [Офіційна документація зчитування та запису в файл на Clojure](https://clojure.org/guides/reading_writing)
- [Повна документація бібліотеки clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Офіційна документація зчитування та запису CSV-файлів на Clojure](https://github.com/clojure-csv/clojure-csv/wiki/How-to-use-clojure-csv)