---
title:                "Kotlin: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Для чого: Конвертація дати в рядок є важливою задачею у програмуванні, оскільки дата є важливою інформацією у багатьох програмах, а потрібна форматування може різнитися в залежності від вимог проекту.

Як: Для конвертації дати в рядок у Kotlin використовуються методи `format()` та `SimpleDateFormat()`. Наприклад:

```Kotlin
val date = Date()
val dateFormat = SimpleDateFormat("dd/MM/yyyy")
val dateString = dateFormat.format(date)

println(dateString) // Виведе: 23/04/2021
```

Глибше: Окрім стандартного формату дати, у Kotlin є можливість використовувати різні шаблони для форматування дати, такі як `yyyy-MM-dd` або `dd MMM yyyy`. Також, можна використовувати метод `parse()` для конвертації рядка у дату. Зверніть увагу, що стандартний формат дати в Kotlin відрізняється від стандартного формату в Java.

Дивіться також: 
- [Офіційна документація Kotlin](https://kotlinlang.org/docs/datetime.html)
- [Гайд з форматування дати у Kotlin](https://www.javatpoint.com/kotlin-string-format-date)
- [Презентація про дати та час у Kotlin](https://www.slideshare.net/jetbrains/whats-new-in-kotlin-18-dates-and-times)