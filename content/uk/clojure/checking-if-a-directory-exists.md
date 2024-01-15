---
title:                "Перевірка існування каталогу"
html_title:           "Clojure: Перевірка існування каталогу"
simple_title:         "Перевірка існування каталогу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії - це важлива завдання для програмістів, які працюють з файловою системою. Це допомагає забезпечити правильну роботу програми та уникнути помилок, пов'язаних з неіснуючими директоріями.

## Як

Щоб перевірити, чи існує директорія, використовуйте функцію `exist?` з вбудованого модуля `clojure.java.io`.

```Clojure
(require '[clojure.java.io :as io])

(io/exists? "path/to/directory") ; поверне true, якщо директорія існує
(io/exists? "path/to/nonexistent/dir") ; поверне false, якщо директорія не існує
```

## Глибший аналіз

Функція `exist?` перевіряє наявність файлу або директорії за вказаним шляхом, повертаючи логічне значення `true` або `false`. Вона також автоматично розрізняє між абсолютним та відносним шляхом, тому ви можете використовувати будь-який із них без проблем.

## Дивіться також

- [Офіційна документація Clojure](https://clojure.org/) 
- [Документація по модулю clojure.java.io](https://clojuredocs.org/clojure.java.io/exists_q) 
- [Огляд файлових операцій в Clojure](https://www.baeldung.com/clojure-file-operations)