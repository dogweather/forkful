---
title:                "Перевірка наявності директорії"
html_title:           "Clojure: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо нам це потрібно?

Перевірка наявності директорії висвітлює, дійсно існує чи не існує директорія за вказаним шляхом. Програмісти використовують це, щоб запобігти помилкам, які виникають, коли вони намагаються працювати з неіснуючою директорією.

## Як це робити:

```Clojure 
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (-> path
      (io/file)
      (.exists)
      (and (.isDirectory (io/file path)))))

(defn test-directory-exists []
  (println (directory-exists? "/path/to/directory"))
  (println (directory-exists? "/path/to/nonexistent/directory")))

(test-directory-exists)
```
Виведення коду:

```Clojure 
true
false
```

## Пірнання глибше:

Історично в Clojure така функція не була передбачена за замовчуванням, тому ми використовуємо java.io.File з Java для цього. Альтернативою є використання бібліотеки third-party.

Основна ідея реалізації полягає в тому, щоб створити об'єкт Java File і перевірити його наявність за допомогою методу .exists. Тоді, якщо файл існує, ми використовуємо метод .isDirectory, щоб перевірити, чи це директорія.

## Дивіться також:

1. Clojure - Java Interop: https://clojure.org/reference/java_interop
2. Java IO File: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
3. Clojure Cookbook - Checking Whether a File or Directory Exists: https://www.programming-books.io/essential/clojure/1b76384c5f5e4f84a94bdbd32919c1e5-checking-whether-a-file-or-directory-exists