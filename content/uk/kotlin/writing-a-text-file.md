---
title:                "Kotlin: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу є важливим етапом у багатьох програмах, таких як збереження даних або налаштування програми. Також це може бути зручним способом для збереження текстових даних для подальшого використання.

## Як

Для написання текстового файлу у Kotlin необхідно використовувати клас `FileWriter` та метод `write()`. Приклад коду з використанням цих засобів можна знайти нижче:

```Kotlin
val file = FileWriter("file.txt")
file.write("Привіт світе!")
file.close()
```

Цей код створить файл з назвою `file.txt` та запише у нього рядок "Привіт світе!". Після виконання програми, у вашій робочій директорії з'явиться текстовий файл з цим рядком.

## Глибше

Клас `FileWriter` є частиною JDK, що означає, що він доступний вам за замовчуванням при використанні Kotlin. Цей клас містить багато методів для роботи з файлами, таких як `append()`, `flush()`, `close()` та інші. Крім того, ви можете також використовувати методи класу `File` для отримання інформації про файл, такої як розмір, дата зміни і т.д.

## Дивись також

- [Документація з Kotlin про FileWriter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file-writer.html)
- [Приклади використання FileWriter](https://www.geeksforgeeks.org/filewriter-class-in-kotlin-with-examples/)
- [Бібліотека kotlinx-io для роботи з файлами у Kotlin](https://github.com/Kotlin/kotlinx-io)