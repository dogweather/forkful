---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-01-20T17:38:56.746032-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-string-to-lower-case.md"
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