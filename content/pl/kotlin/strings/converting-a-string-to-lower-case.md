---
date: 2024-01-20 17:38:56.746032-07:00
description: "How to: (Jak to zrobi\u0107:) Historia zmiany wielko\u015Bci liter si\u0119\
  ga czas\xF3w przed informatyk\u0105, gdzie r\u0119czne przepisywanie tekst\xF3w\
  \ wymusza\u0142o ujednolicenie\u2026"
lastmod: '2024-04-05T22:50:49.669048-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Historia zmiany wielko\u015Bci liter si\u0119ga czas\xF3\
  w przed informatyk\u0105, gdzie r\u0119czne przepisywanie tekst\xF3w wymusza\u0142\
  o ujednolicenie wygl\u0105du dokument\xF3w."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## How to: (Jak to zrobić:)
```kotlin
fun main() {
    val originalString = "Witaj Świecie!"
    val lowerCaseString = originalString.lowercase()

    println("Original: $originalString")
    println("Lowercase: $lowerCaseString")
}
```
Output:
```
Original: Witaj Świecie!
Lowercase: witaj świecie!
```

## Deep Dive (Dogłębna Analiza)
Historia zmiany wielkości liter sięga czasów przed informatyką, gdzie ręczne przepisywanie tekstów wymuszało ujednolicenie wyglądu dokumentów. W Kotlinie metoda `lowercase()` zastąpiła starszą `toLowerCase()`, wprowadzając poprawną obsługę wszystkich znaków Unicode, w tym specyficznych dla języka polskiego. Alternatywą może być samodzielne mapowanie każdej litery z użyciem własnej funkcji, ale nie jest to zalecane ze względu na złożoność i wydajność. Implementacja `lowercase()` korzysta z pewnych trików Unicode i algorytmów normalizujących, by obsłużyć różne przypadki, takie jak ligatury, czy specjalne znaki alfabetów niełacińskich.

## See Also (Zobacz również)
- Dokumentacja Kotlin na temat `lowercase()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html
- Historia Unicode: https://home.unicode.org/
- Porównanie metod `lowercase()` i `toLowerCase()`: https://kotlinlang.org/docs/whatsnew14.html#better-charsequence-handling
