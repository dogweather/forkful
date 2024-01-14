---
title:                "Kotlin: Перетворення рядка в нижній регістр."
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Конвертація рядка до малого регістру дуже важлива для багатьох програмістів, оскільки це дозволяє більш ефективно працювати з рядками і забезпечує їх порівняння і сортування.

## Як зробити

Існує декілька способів конвертувати рядок до малого регістру в Kotlin. Найпростіший і найбільш ефективний спосіб це використання методу `toLowerCase()`:

```Kotlin
val str = "КОТЛИН"
val lowerCaseStr = str.toLowerCase()

println(lowerCaseStr) // виведе "котлин"
```

Також можна використовувати метод `convertCase()` з бібліотеки `kotlin.text`:

```Kotlin
val str = "КоТЛиН"
val lowerCaseStr = str.convertCase(TextDirection.LOWERCASE)

println(lowerCaseStr) // виведе "котлин"
```

## Глибоке вивчення

У Kotlin існує різниця між методами `toLowerCase()` та `convertCase()` оскільки вони використовують різні мовні бібліотеки. Метод `toLowerCase()` використовує мовну бібліотеку Java, тоді як метод `convertCase()` використовує бібліотеку Kotlin. Це може вплинути на ефективність і результати, тому рекомендуємо спробувати обидва методи і вибрати той, який підходить для вашої програми краще.

## Дивись також

- [Документація Kotlin про метод `toLowerCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/to-lower-case.html)
- [Документація Kotlin про метод `convertCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/convert-case.html)
- [Стаття про роботу з рядками в Kotlin](https://progtom.com/2017/12/03/kotlin-%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%B0%D0%B5%D0%BC-%D1%81-%D1%80%D1%8F%D0%B4%D0%BA%D0%B0%D0%BC%D0%B8/)