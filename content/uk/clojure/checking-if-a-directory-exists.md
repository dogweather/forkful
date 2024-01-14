---
title:                "Clojure: Перевірка існування директорії"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії є важливою частиною програмування, оскільки це дозволяє перевірити наявність необхідних файлів та директорій перед виконанням деяких операцій, що зменшує ризик виникнення помилок у програмі.

## Як це зробити

Для перевірки, чи існує директорія, можна використати функцію `file-seq`, яка повертає послідовність файлів та директорій у вказаній шляху. Наприклад:

```Clojure
(def directory "/home/user/example/dir")

(def files (file-seq directory))

(println files)
```

Вище вказане приклад виведе на екран всі файли та директорії, що знаходяться у шляху `/home/user/example/dir`.

## Глибоке занурення

Функція `file-seq` також може бути використана з путем до файлу або символьним посиланням. Це означає, що ви можете перевірити наявність конкретного файлу, а не просто директорії. Наприклад:

```Clojure
(def file "/home/user/example/dir/test.txt")

(def files (file-seq file))

(println files)
```

Цей код виведе на екран інформацію про файл `test.txt`, якщо він існує у зазначеній директорії.

## Дивись також

- [Офіційна документація Clojure](https://clojure.org/)
- [Як зробити перевірку наявності файлу у Clojure](https://stackoverflow.com/questions/15985110/how-to-check-if-a-file-exists-in-clojure)
- [Приклади використання `file-seq` у Clojure](https://gist.github.com/aaronbieber/2470157)