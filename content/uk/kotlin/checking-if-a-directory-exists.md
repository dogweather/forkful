---
title:                "Перевірка наявності директорії"
html_title:           "Bash: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Перевірка наявності директорії - це процес виявлення, чи існує певна директорія на вашому файловому системі. Програмісти роблять це, щоб уникнути помилок читання або запису, які можуть статися, якщо директорія не існує.

## Як це зробити:
```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val directoryPath = Paths.get("/path/to/your/directory")

    val directoryExists = Files.exists(directoryPath)

    if (directoryExists) {
        print("Directory Exists")
    } else {
        print("Directory Does not Exists")
    }
}
```
У цьому прикладі ми використовуємо пакет `java.nio.file` для перевірки наявності директорії. 

## Пірнання вглиб:
Перевірка наявності директорії не є новим поняттям у програмуванні. Вона використовується вже декілька десятиліть, оскільки є необхідною для роботи з файлами і директоріями.

Як альтернативу, ви можете використовувати `java.io.File`, але `java.nio.file.Paths` і `java.nio.file.Files` є більш новим та гнучкішим рішенням, яке надає більше можливостей.

Важливо пам'ятати, що оператори I/O можуть викликати помилки, отже переконайтесь, що ваш код враховує це, використовуючи блоки try-catch для обробки таких помилок.

## Див. також:
- Докладніше про `java.nio.file.Files`: [Oracle Docs - Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- Про обробку помилок в Kotlin: [Handling Exceptions in Kotlin](https://kotlinlang.org/docs/exceptions.html) 