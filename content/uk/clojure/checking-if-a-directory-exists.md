---
title:                "Перевірка наявності директорії"
date:                  2024-01-19
simple_title:         "Перевірка наявності директорії"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Перевірка існування директорії - це спосіб встановити, чи є вказаний шлях реальною папкою. Програмісти роблять це, щоб уникнути помилок під час читання чи запису файлів.

## Як це зробити:
```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (let [file (io/file path)]
    (and (.exists file) (.isDirectory file))))

;; Usage
(println (directory-exists? "/path/to/directory")) ;; true or false
```
Приклад результату:
```
true
```
або
```
false
```

## Поглиблений Розгляд:
Спосіб перевірки існування директорії змінювався з часом та залежав від операційної системи. У Clojure така перевірка спрощена завдяки JVM, яка забезпечує однакову поведінку незалежно від платформи. Стандартною альтернативою є використання команди `ls` в Unix-подібних системах або `dir` у Windows через виклик з системного шела, але це менш портативно і може бути менш ефективним. З іншого боку, використання Java interop в Clojure дозволяє ефективно працювати з файловою системою.

## Дивіться Також:
- Clojure `clojure.java.io` [documentation](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Java `File` class [documentation](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/io/File.html)
- StackOverflow discussion on file system interactions in Clojure: [link](https://stackoverflow.com/questions/tagged/clojure+file-io)
