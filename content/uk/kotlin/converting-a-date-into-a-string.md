---
title:                "Kotlin: Перетворення дати у рядок"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Чому

Форматування дати у строку є важливою задачею для багатьох програмістів, оскільки це дозволяє зручно працювати з датами у програмах. На щастя, Kotlin має декілька потужних інструментів, які полегшують процес конвертації дати у строку.

##Як конвертувати дату у строку

Для конвертації дати у строку у Kotlin, вам потрібно використовувати метод `format()` з об'єкта `DateTimeFormatter`, що належить до пакету `java.time`. Нижче наведено приклад коду:

```Kotlin
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
fun main() { 
    val currentDateTime = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formattedDate = currentDateTime.format(formatter)
    println(formattedDate) // Виводить дату у форматі "yyyy-MM-dd HH:mm:ss" 
}
```

В результаті ви отримаєте поточну дату у зазначеному форматі. Ви також можете використовувати різні шаблони форматування для отримання дати у бажаному вигляді.

##Глибше занурення

Крім стандартних шаблонів форматування, Kotlin також дозволяє використовувати змінні для створення власних шаблонів форматування. Наприклад, ви можете використовувати `%t` для вставки дати та часу з одного об'єкта `Instant` у строку.

Крім того, ви можете зберігати різні шаблони форматування у власних змінних та використовувати їх при необхідності. Це дозволить зробити ваш код більш читабельним та покращить його сумісність з різними версіями Kotlin.

##Дивіться також

- [Документація з DateTimeFormatter](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/format/DateTimeFormatter.html)
- [Generic Guide to Dates and Time](https://www.delaree.com/blog/generic-guide-to-dates-and-time/)
- [Повне руководство з Kotlin](https://kotlinlang.org/docs/kotlin-docs.pdf)