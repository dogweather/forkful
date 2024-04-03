---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:26.985104-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435\
  \ `clojure.java.io/file` \u0434\u043B\u044F \u0441\u043E\u0437\u0434\u0430\u043D\
  \u0438\u044F \u043E\u0431\u044A\u0435\u043A\u0442\u0430 File \u0438 `.exists` \u0434\
  \u043B\u044F \u043F\u0440\u043E\u0432\u0435\u0440\u043A\u0438 \u0435\u0433\u043E\
  \ \u0441\u0443\u0449\u0435\u0441\u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F\
  . \u041C\u0435\u0442\u043E\u0434 `isDirectory`\u2026"
lastmod: '2024-03-13T22:44:44.376691-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 `clojure.java.io/file`\
  \ \u0434\u043B\u044F \u0441\u043E\u0437\u0434\u0430\u043D\u0438\u044F \u043E\u0431\
  \u044A\u0435\u043A\u0442\u0430 File \u0438 `.exists` \u0434\u043B\u044F \u043F\u0440\
  \u043E\u0432\u0435\u0440\u043A\u0438 \u0435\u0433\u043E \u0441\u0443\u0449\u0435\
  \u0441\u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F."
title: "\u041F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u0441\u0443\u0449\u0435\u0441\
  \u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\
  \u043E\u0440\u0438\u0438"
weight: 20
---

## Как это сделать:
Используйте `clojure.java.io/file` для создания объекта File и `.exists` для проверки его существования. Метод `isDirectory` подтверждает, является ли File директорией.

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (let [dir (io/file path)]
    (and (.exists dir) (.isDirectory dir))))

;; Пример использования:
(directory-exists? "/path/to/directory") ;=> true или false
```
Пример вывода:
```
true ; если директория существует
false ; если директория не существует
```

## Подробнее
Исторически подобный процесс используется в Java; поскольку Clojure работает на JVM, она использует библиотеки Java для операций с файловой системой. Альтернативы в Clojure могут включать использование других функций или библиотек Java, таких как `nio.file.Files`. Под капотом проверка существования директории может быть интенсивной с точки зрения ввода-вывода и может вести себя по-разному на разных операционных системах и при разных разрешениях файловой системы, поэтому подтверждение её существования перед выполнением дальнейших операций крайне важно.

## Смотрите также
- Документация Clojure по вводу-выводу: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Класс File в Java: [https://docs.oracle.com/javase/8/docs/api/java/io/File.html](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Класс Files NIO в Java: [https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
