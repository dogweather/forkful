---
title:                "Kotlin: Створення тимчасового файлу"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому:
Створення тимчасового файлу дуже зручно та корисно для збереження тимчасових даних під час виконання програми.

## Як:
```Kotlin
val tempFile = File.createTempFile("temp", ".txt")
println(tempFile.path)
```

Вищевказаний код створить тимчасовий файл з іменем "temp" та розширенням ".txt". Після цього, виведе шлях до файлу, який можна використовувати для збереження даних.

## Deep Dive:
Створення тимчасового файлу є досить простою операцією, але варто знати підходи для більш гнучкого використання. Наприклад, можна вказати власну директорію для збереження тимчасових файлів, або використовувати префікс та суфікс для створення унікального імені файлу. Також, варто пам'ятати про необхідність звільняти ресурси після використання тимчасового файлу.

## Дивитись також:
- [Документація Kotlin зі створення тимчасових файлів](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/create-temp-file.html)
- [Порівняння з створенням тимчасових файлів у Java](https://www.baeldung.com/java-temporary-files)