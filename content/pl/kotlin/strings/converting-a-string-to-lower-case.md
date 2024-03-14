---
date: 2024-01-20 17:38:56.746032-07:00
description: "Zamiana \u0142a\u0144cucha znak\xF3w na ma\u0142e litery to proces,\
  \ gdzie wszystkie wielkie litery w tek\u015Bcie staj\u0105 si\u0119 ma\u0142ymi.\
  \ Programi\u015Bci u\u017Cywaj\u0105 tej zmiany dla unifikacji\u2026"
lastmod: '2024-03-13T22:44:35.351382-06:00'
model: gpt-4-1106-preview
summary: "Zamiana \u0142a\u0144cucha znak\xF3w na ma\u0142e litery to proces, gdzie\
  \ wszystkie wielkie litery w tek\u015Bcie staj\u0105 si\u0119 ma\u0142ymi. Programi\u015B\
  ci u\u017Cywaj\u0105 tej zmiany dla unifikacji\u2026"
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zamiana łańcucha znaków na małe litery to proces, gdzie wszystkie wielkie litery w tekście stają się małymi. Programiści używają tej zmiany dla unifikacji danych, porównywania łańcuchów bez uwzględnienia wielkości liter lub przygotowania tekstu do wyświetlenia w jednolitym formacie.

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
