---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:57:37.469046-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Перевірка існування каталогу — це процес визначення, чи існує певний каталог у файловій системі. Програмісти виконують це, щоб уникнути помилок перед виконанням операцій із файлами чи каталогами.

## Як це зробити:
Приклад коду Kotlin, який показує як перевірити існування каталогу:

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/path/to/directory")
    
    if (Files.exists(path)) {
        println("Каталог існує.")
    } else {
        println("Каталог не знайдено.")
    }
}
```

Просто замініть `/path/to/directory` шляхом до вашого каталогу.

## Поглиблений Розгляд:
Історично, перевірка існування каталогів у Java могла бути реалізована за допомогою класу `File`. Але з Java 7 і далі, рекомендують використовувати `java.nio.file` пакет, який надає більшу гнучкість і читабельність коду.

Альтернативою методу `Files.exists()` є `Files.notExists()`, який явно перевіряє відсутність каталогу. Важливо враховувати, що оба методи можуть давати невизначену поведінку, якщо доступ до шляху обмежено.

При перевірці існування каталогу також слід враховувати можливість гонки умов (race condition), коли стан файлової системи може змінитися після того, як було виконано перевірку.

## Дивіться Також:
- [Java NIO File API](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Kotlin Documentation](https://kotlinlang.org/docs/reflection.html#function-references)
