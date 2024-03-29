---
date: 2024-01-20 17:55:05.256135-07:00
description: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432 Kotlin - \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u0437 \u0444\u0430\u0439\u043B\
  \u0430, \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043E\u0433\u043E \u043D\
  \u0430 \u0434\u0438\u0441\u043A\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  , \u0449\u043E\u0431 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437\
  \ \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0454\u044E,\u2026"
lastmod: '2024-03-13T22:44:49.248220-06:00'
model: gpt-4-1106-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432 Kotlin - \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u0437 \u0444\u0430\u0439\u043B\
  \u0430, \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043E\u0433\u043E \u043D\
  \u0430 \u0434\u0438\u0441\u043A\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  , \u0449\u043E\u0431 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437\
  \ \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0454\u044E,\u2026"
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
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
