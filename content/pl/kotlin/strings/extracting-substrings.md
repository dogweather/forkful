---
date: 2024-01-20 17:46:16.521177-07:00
description: "Wyodr\u0119bnianie podci\u0105g\xF3w to proces wycinania mniejszych\
  \ ci\u0105g\xF3w znak\xF3w z wi\u0119kszego. Programi\u015Bci robi\u0105 to, by\
  \ manipulowa\u0107 danymi tekstowymi \u2013 weryfikowa\u0107,\u2026"
lastmod: 2024-02-19 22:04:54.484350
model: gpt-4-1106-preview
summary: "Wyodr\u0119bnianie podci\u0105g\xF3w to proces wycinania mniejszych ci\u0105\
  g\xF3w znak\xF3w z wi\u0119kszego. Programi\u015Bci robi\u0105 to, by manipulowa\u0107\
  \ danymi tekstowymi \u2013 weryfikowa\u0107,\u2026"
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wyodrębnianie podciągów to proces wycinania mniejszych ciągów znaków z większego. Programiści robią to, by manipulować danymi tekstowymi – weryfikować, analizować, czy formatować je według potrzeb.

## How to: (Jak to zrobić:)
```kotlin
fun main() {
    val text = "Witaj, Kotlin!"

    val sub1 = text.substring(7)       // Wyodrębni od znaku na pozycji 7: "Kotlin!"
    val sub2 = text.substring(0, 6)    // Wyodrębni zakres od 0 do 5 włącznie: "Witaj"

    println(sub1)
    println(sub2)
}

// Output:
// Kotlin!
// Witaj
```

```kotlin
fun main() {
    val quote = "Być, albo nie być, oto jest pytanie."

    val toBeOrNotToBe = quote.substring(0..2) + quote.substring(7..9) // "Być, nie być"
    println(toBeOrNotToBe)
}

// Output:
// Być, nie być
```

## Deep Dive (Solidne nurkowanie):
W Kotlinie i wielu innych językach programowania, wyodrębnianie podciągów to podstawowe narzędzie do pracy z tekstem. Historycznie rzecz biorąc, pochodzi z języków jak C, gdzie obsługa ciągów była bardziej manualna.

Alternatywą dla `substring` może być użycie wyrażeń regularnych (`Regex`), które są mocnym narzędziem, jeśli potrzeba bardziej złożonych operacji na tekstach.

Jeżeli chodzi o implementację, `substring` w Kotlinie (i w JVM) jest zazwyczaj efektywny - nowy ciąg dzieli tablicę znaków z oryginalnym ciągiem, nie tworząc niepotrzebnie nowej kopii każdego znaku.

## See Also (Zobacz również):
- [Dokumentacja Kotlin - Substring](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Dokumentacja Kotlin - Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
