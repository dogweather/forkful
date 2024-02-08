---
title:                "Проверка существования директории"
date:                  2024-01-28T23:55:26.985104-07:00
model:                 gpt-4-0125-preview
simple_title:         "Проверка существования директории"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Проверка наличия директории означает подтверждение того, указывает ли путь на директорию в вашей файловой системе. Программисты делают это, чтобы предотвратить ошибки, обеспечить правильную обработку файлов и настроить необходимые условия перед выполнением файловых операций.

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
