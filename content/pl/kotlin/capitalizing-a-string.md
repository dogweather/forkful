---
title:    "Kotlin: Konwertowanie ciągu na wielkie litery"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jednym z często wykorzystywanych zadań w programowaniu jest konieczność zmiany wielkości liter w ciągu znaków. Dotyczy to szczególnie języków, gdzie obsługa polskich znaków jest istotna dla poprawnego wyświetlania tekstu. Dlatego warto poznać możliwości języka Kotlin w kwestii zmiany wielkości liter w ciągach znaków.

## Jak to zrobić

W języku Kotlin istnieje kilka sposobów na konwertowanie ciągu znaków do dużych lub małych liter. Można wykorzystać metody dostępne w klasie String, a także skorzystać z funkcji zdefiniowanych w rozszerzeniach (ang. extensions). Poniżej przedstawione są przykłady kodów wraz z oczekiwanym wyjściem.

```Kotlin
// wykorzystanie metody toUpperCase()

val text = "kotlin jest Fajny!"
println(text.toUpperCase())

// wyjście: KOTLIN JEST FAJNY!
```

```Kotlin
// wykorzystanie metody toLowerCase()

val text = "TaK właśnie widczasz!"
println(text.toLowerCase())

// wyjście: tak właśnie widczasz!
```

```Kotlin
// wykorzystanie funkcji extenstion toTitleCase()

fun String.toTitleCase(): String {
    return this.split(" ").joinToString(" ") { it.toLowerCase().capitalize() }
}

val text = "kotlin jest naprawdę super!"
println(text.toTitleCase())

// wyjście: Kotlin Jest Naprawdę Super!
```

## Dogłębna analiza

Powyższe przykłady pokazują podstawowe sposoby na zmianę wielkości liter w ciągu znaków. Istnieją jednak dodatkowe możliwości, na przykład wykorzystanie metody replace() w połączeniu z wyrażeniami regularnymi. Warto także pamiętać o uwzględnieniu polskich znaków i wykorzystywaniu odpowiednich kodowań.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o manipulacji ciągami znaków w języku Kotlin, warto sprawdzić poniższe linki:

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/strings.html
- Blog o programowaniu w języku Kotlin: https://blog.jetbrains.com/kotlin/
- Tutorial o manipulacji ciągami znaków w Kotlin: https://www.baeldung.com/kotlin-string-manipulation