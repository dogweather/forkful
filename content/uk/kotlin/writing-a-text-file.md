---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що та чому?
Запис текстового файлу - це процес збереження даних у форматі, який може бути прочитаним людиною. Програмісти роблять це для постійного зберігання інформації, обміну даними між програмами або для логування подій програми.

## Як це зробити:
Запис текстового файлу в Kotlin виконується кількома способами. Нижче представлено два приклади:

```Kotlin
import java.io.File

fun main() {
    // Спосіб 1: Використання File.writeText()
    val data = "Привіт, світ!"
    File("output.txt").writeText(data)

    // Спосіб 2: Використання bufferedWriter()
    File("output.txt").bufferedWriter().use { writer ->
        writer.write(data)
    }
}
```
Після запуску цього коду, ви отримаєте файл "output.txt" з текстом "Привіт, світ!".

## Занурення у контекст:
Історично, запис файлів виконувався через нижньорівневі ввід/вивід операції, і досі ці механізми лежать в основі файлових операцій високого рівня. Альтернативою є використання FileOutputStream або FileWriter для більш деталізованого контролювання запису. Однак, Kotlin спрощує процес зручнішими функціями через стандартную бібліотеку.

## Дивіться також:
- [Офіційна документація Kotlin з файлових операцій](https://kotlinlang.org/docs/idioms.html#read-contents-of-a-file)
- [Документація Java File I/O](https://docs.oracle.com/javase/tutorial/essential/io/) - оскільки Kotlin повністю сумісний з Java, це теж корисно.
- [Stack Overflow: Як записати файл в Kotlin](https://stackoverflow.com/questions/4646577/global-exception-handling-in-kotlin) - практичні поради та приклади від інших розробників.
