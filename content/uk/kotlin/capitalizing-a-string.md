---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Коли ми кажемо про "капіталізацію рядків", маємо на увазі зміну першої літери слова на велику. Програмісти це роблять для форматування тексту, наприклад, на початку речень або для назв.

## Як це зробити:
```kotlin
fun main() {
    val originalText = "карпати - перлина україни."
    val capitalizedText = originalText.split(" ").joinToString(" ") { it.capitalize() }
    println(capitalizedText)
}

// Вивід:
// Карпати - Перлина України.
```
Метод `capitalize()` замінюється на `replaceFirstChar` у нових версіях Kotlin:
```kotlin
fun String.capitalizeFirstLetter(): String = 
    this.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }

fun main() {
    val originalText = "львів - столиця кави."
    val capitalizedText = originalText.split(" ").joinToString(" ") { it.capitalizeFirstLetter() }
    println(capitalizedText)
}

// Вивід:
// Львів - Столиця Кави.
```

## Глибинне занурення
Раніше в Kotlin для капіталізації використовувався метод `capitalize()`, але через його неоднозначність - не коректну роботу з різними мовами та культурами, від версії 1.5 його замінено методом `replaceFirstChar`. "Title case" у більшості мов відноситься до трансформації першої літери слова у велику літеру. Важливо, що різні мови мають різні правила капіталізації.

Щодо альтернатив, ви також можете використовувати бібліотеки сторонніх розробників або регулярні вирази, хоча для більшості потреб вбудовані засоби Kotlin виявляться досить зручними.

## Дивіться також
- Kotlin Documentation on Strings: [https://kotlinlang.org/docs/reference/basic-types.html#strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- Kotlin Standard Library: [https://kotlinlang.org/api/latest/jvm/stdlib/](https://kotlinlang.org/api/latest/jvm/stdlib/)
- Unicode Standard Annex #29: Text Boundaries: [https://unicode.org/reports/tr29/#Word_Boundaries](https://unicode.org/reports/tr29/#Word_Boundaries), який може бути корисний для розуміння, як робити капіталізацію враховуючи різні мови.
