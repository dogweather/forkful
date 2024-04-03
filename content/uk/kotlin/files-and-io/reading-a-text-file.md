---
date: 2024-01-20 17:55:05.256135-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:49.248220-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
