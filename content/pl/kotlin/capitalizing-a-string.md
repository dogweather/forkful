---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Kotlin: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zamiana tekstu na duże litery to proces przekształcania wszystkich liter małych w stringu na duże. Programiści robią to aby ułatwić odczyt i sortowanie tekstów, czy też do normalizacji danych.

## Jak to zrobić:

W Kotlinie, operację tę wykonujemy za pomocą funkcji `toUpperCase()`. 

```kotlin
fun main() {
    val s = "witaj, świecie"
    val result = s.toUpperCase()

    println(result) // Wypisuje: WITAJ, ŚWIECIE
}
```

Prosty i skuteczny sposób, prawda?

## Pogłębiamy temat

1. Kontekst historyczny: Przeważnie w starszych językach programowania istniały podobne funkcje. Często nazywały się `uppercase()` lub `toUpper()`.
2. Alternatywy: Jeśli chcesz tylko kapitalizować pierwszą literę w stringu, użyj `capitalize()`. W przypadku pierwszej litery każdego słowa - `capitalizeEachWord()`.
3. Szczegóły Implementacji: `toUpperCase()` w Kotlinie korzysta z metody `Character.toUpperCase(int codePoint)` której działanie jest zgodne z Unicode Case Mapping.

```kotlin
fun main() {
    val s = "witaj, świecie"
    val firstLetterCap = s.capitalize()
    val eachWordCap = s.split(" ").joinToString(" ") { it.capitalize() }

    println(firstLetterCap) // Wypisuje: Witaj, świecie
    println(eachWordCap) // Wypisuje: Witaj, Świecie
}
```

## Zobacz również 

Jest dużo źródeł na temat operacji na stringach w Kotlinie. 

1. [Dokumentacja Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)
2. [Unicode Case Mapping](https://unicode.org/reports/tr21/)
3. [Dokumentacja Kotlin na temat `capitalize()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
4. [Dokumentacja Kotlin na temat `toUpperCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)

Pamiętaj! Znajomość operacji na stringach jest kluczowa dla każdego programisty. Sprawdź powyższe linki!