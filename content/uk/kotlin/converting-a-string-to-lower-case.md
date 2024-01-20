---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?
Переведення рядка в нижній регістр - це процес заміни всіх великих літер на маленькі. Програмісти роблять це, щоб уникнути проблем з чутливістю до регістру при порівнянні рядків.

## Як це робиться:
У Kotlin це можна зробити дуже просто за допомогою `toLowerCase()`. Ось код і приклад виводу:

```kotlin
val greeting = "Привіт, Світ!"
val lowerCaseGreeting = greeting.toLowerCase()
println(lowerCaseGreeting) // вивести: привіт, світ!
```
Так само, якщо ми маємо рядок з англійською мовою, результат все одно коректний:

```kotlin
val englishGreeting = "Hello, World!"
val lowerCaseEnglishGreeting = englishGreeting.toLowerCase()
println(lowerCaseEnglishGreeting) // вивести: hello, world!
```

## Поглиблений огляд:
Змінена регістрація: ідея заміни великих літер на маленькі існує стільки, скільки і комп'ютерна наука. Проте, існують альтернативи, наприклад, `toLowerCase(Locale.ROOT)`, який стосується поділу на словник, або `toUpperCase()`, якщо вам потрібно перетворити все на великі літери.

Під капотом, `toLowerCase()` працює, переміщаючи через кожен символ у рядку, перевіряючи, чи є він великою літерою, і якщо так, замінюючи його на відповідну маленьку літеру.

## Дивіться також:
- [Документація Kotlin про рядки](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)