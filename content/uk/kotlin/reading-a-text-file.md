---
title:                "Читання текстового файлу"
aliases:
- uk/kotlin/reading-a-text-file.md
date:                  2024-01-20T17:55:05.256135-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Читання текстового файла в Kotlin - це процес отримання даних з файла, збереженого на диску. Програмісти роблять це, щоб працювати з інформацією, збереженою в текстовій формі, - налаштування, дані, журнали чи просто текст для обробки.

## Як це зробити:
```Kotlin
import java.io.File

fun main() {
    val filePath = "path/to/your/file.txt"
    
    val text = File(filePath).readText(Charsets.UTF_8)
    println(text)
}
```
*Sample output:*
```
Hello, file reader in Ukraine!
Гарного дня!
```

Використання `readLines` для ітерації по строках:
```Kotlin
import java.io.File

fun main() {
    val filePath = "path/to/your/file.txt"
    
    File(filePath).useLines { lines ->
        lines.forEach { println(it) }
    }
}
```

## Поглиблений огляд
У світі програмування, читання файлів – це основа. В Kotlin це робиться легко, завдяки своїй стандартній бібліотеці. Історично, Java потребувала багато коду для цієї задачі, але Kotlin спростив процес. 

Альтернативи:
- `BufferedReader`: добре для великих файлів.
- `Scanner`: коли потрібно більше контролю над парсингом.

Деталі реалізації:
- `readText` і `useLines` викидають виняток `IOException` при помилках з файлом. Використовуйте `try-catch`, аби уникнути збоїв програми.
- `Charsets.UTF_8` важливо вказати для правильної роботи із символами української мови.

## Дивіться також:
- [Kotlin Documentation on Reading Files](https://kotlinlang.org/docs/idioms.html#read-file) – офіційна документація Kotlin.
