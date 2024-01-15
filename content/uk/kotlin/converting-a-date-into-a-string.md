---
title:                "Перетворення дати в рядок"
html_title:           "Kotlin: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Конвертація дати в строку є дуже корисним процесом в програмуванні, який дозволяє зробити дати більш зрозумілими для користувачів і забезпечити зручну взаємодію із даними.

## Як

```Kotlin
// Створюємо об'єкт типу LocalDataTime з поточною датою та часом 
val currentDateTime = LocalDateTime.now()

// Конвертуємо дату у строку за допомогою методу toString(),
// передаючи формат, в якому хочемо отримати дату у вигляді строкі
val stringDate = currentDateTime.toString("dd-MM-yyyy")

// Виводимо отриману строку на екран
println("Поточна дата у форматі dd-MM-yyyy: $stringDate") 

// Вихід: Поточна дата у форматі dd-MM-yyyy: 29-07-2021
```

## Глибше занурення

У Kotlin є декілька варіантів конвертації дати в строку: за допомогою методу toString(), за допомогою використання форматувальників (DateTimeFormatter) або за допомогою функції format(). Кожен з цих методів має свої особливості та може бути використаний залежно від конкретної задачі.

## Дивіться також

- [Документація Kotlin про конвертування дати в строку](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date-time/to-string.html)
- [Стаття на тему форматування дати в Kotlin](https://developer.android.com/guide/topics/ui/look-and-feel/internationalization#date-formats)
- [Підручник з Kotlin](https://kotlinlang.org/docs/home.html)