---
title:                "Перевірка наявності каталогу"
html_title:           "Clojure: Перевірка наявності каталогу"
simple_title:         "Перевірка наявності каталогу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Що і чому?

Перевірка, чи існує директорія, це процес визначення, чи існує введена користувачем директорія в файловій системі. Це важливо для програмістів, щоб переконатися, що кроки, які вони виконують там, де необхідна директорія, мають відповідні налаштування та дозволи.

Як це зробити:

```Clojure
(require '[clojure.java.io :refer [file]])
(file-exists? "/Users/username/Documents")
```
Приклад виходу: true

Глибоке занурення:

Історичний контекст: Щоб перевірити, чи існує директорія, програмісти використовували спеціальні команди оператора системи, такі як "dir" в Windows і "ls" в Unix. З появою функції "file-exists?" в Clojure, цей процес став більш простим та швидким.

Альтернативи: Для перевірки наявності директорії також можна використовувати функцію "file-seq", яка повертає список файлів і папок в директорії, і порівняти його зі списком шуканих директорій.

Деталі реалізації: Ця функція використовує клас "java.io.File" для створення нового екземпляра файлу та перевіряє його наявність за допомогою методу "exists". Код функції доступний для перегляду на сайті Github Clojure.

Дивіться також:

Документація Clojure: https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file-exists. 

Стаття на тему перевірки існування файлів та директорій: https://www.baeldung.com/java-check-file-exists. 

Код функції на Github: https://github.com/clojure/clojure/blob/master/src/clj/clojure/java/io.clj#L190.