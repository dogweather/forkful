---
date: 2024-01-20 17:55:05.256135-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 \u0441\u0432\u0456\u0442\u0456 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0443\u0432\u0430\u043D\u043D\u044F, \u0447\u0438\u0442\u0430\u043D\u043D\u044F\
  \ \u0444\u0430\u0439\u043B\u0456\u0432 \u2013 \u0446\u0435 \u043E\u0441\u043D\u043E\
  \u0432\u0430. \u0412 Kotlin \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u044C\u0441\
  \u044F \u043B\u0435\u0433\u043A\u043E, \u0437\u0430\u0432\u0434\u044F\u043A\u0438\
  \ \u0441\u0432\u043E\u0457\u0439 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\
  \u043D\u0456\u0439 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u0446\u0456\
  . \u0406\u0441\u0442\u043E\u0440\u0438\u0447\u043D\u043E, Java\u2026"
lastmod: '2024-04-05T22:51:02.339012-06:00'
model: gpt-4-1106-preview
summary: "\u0423 \u0441\u0432\u0456\u0442\u0456 \u043F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0443\u0432\u0430\u043D\u043D\u044F, \u0447\u0438\u0442\u0430\u043D\u043D\
  \u044F \u0444\u0430\u0439\u043B\u0456\u0432 \u2013 \u0446\u0435 \u043E\u0441\u043D\
  \u043E\u0432\u0430."
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
