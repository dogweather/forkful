---
title:                "Clojure: Читання текстового файлу"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Зчитування текстового файлу є важливою навичкою для будь-якого програміста. Це дає можливість працювати зі збереженими даними та обробляти їх за допомогою програмного коду. Знання роботи з текстовими файлами може збільшити продуктивність та ефективність процесу розробки програм.

## Як це зробити

За допомогою мови програмування Clojure, зчитування текстового файлу є досить простим завданням. Для цього необхідно використати функцію "slurp", яка зчитує вміст файлу та повертає його в рядковому форматі.

```Clojure
(def file-contents (slurp "test.txt"))
(println file-contents)
```

Вище наведений приклад демонструє зчитування вмісту з файлу з назвою "test.txt" та виведення його на екран.

```Clojure
(def file-contents (slurp "test.txt"))
; Hello world!

(println (.toUpperCase file-contents))
; HELLO WORLD!
```

У цьому прикладі, зчитаний вміст файлу перетворюється в верхній регістр за допомогою методу ".toUpperCase" та виводиться на екран.

## Глибше в практицю

Тепер, коли ви знаєте як зчитувати текстовий файл за допомогою Clojure, ви можете експериментувати з різними методами та функціями цієї мови для обробки та редагування даних з файлів.

Наприклад, ви можете використати функцію "with-open" для відкриття та закриття файлу:

```Clojure
(with-open [file-reader (clojure.java.io/reader "test.txt")]
  (doall (line-seq file-reader)))
```

Цей приклад поверне список рядків з файлу "test.txt".

## Дивись також

- [Офіційна документація Clojure](https://clojure.org/)
- [Вступ до Clojure для початківців](https://medium.com/@OrestisT17/clojure-for-beginners-introduction-and-basic-functions-386c842b5b66)
- [Робота з файлами в Clojure](https://clojuredocs.org/clojure.java.io/reader)