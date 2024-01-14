---
title:                "Clojure: Перевірка наявності каталогу"
simple_title:         "Перевірка наявності каталогу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Провірка існування папки є важливим кроком в програмуванні. Це дає можливість перевірити наявність файлів або папок, щоб забезпечити плавне виконання програми і уникнути будь-яких помилок.

## Як виконати

```Clojure
(require '[clojure.java.io :as io])

(defn check-directory [directory]
  (if (.isDirectory (io/file directory))
    (println (str "Папка " directory " існує."))
    (println (str "Папки " directory " не існує."))))
```

Викликати цю функцію з певною директорією, щоб перевірити її існування:

```Clojure
(check-directory "/home/user/example_directory")
```

Вихідний текст буде залежати від того, чи існує ця папка. Наприклад:

```Clojure
"Папка /home/user/example_directory існує."
```

або

```Clojure
"Папки /home/user/example_directory не існує."
```

## Глибші дивування

Під час провірки існування папки, ми використовуємо функцію `isDirectory` з бібліотеки `java.io`. Ця функція повертає істину, якщо файл або папку існує або була створена.

Також варто зазначити, що ця функція повертає `false`, якщо файл або папка існує, але вона недоступна для зчитування. Це може стати проблемою при отриманні даних з цієї папки.

## Дивіться також

- [Цікаві функції Clojure для роботи з файлами та папками](https://lambdaduck.com/clojure/working-with-files-and-directories/)
- [Офіційна документація Clojure для бібліотеки `java.io`](https://clojuredocs.org/clojure.java.io)