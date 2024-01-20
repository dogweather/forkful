---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng i programmering betyr å gjøre første bokstav i et ord eller en setning stor (versal). Vi gjør dette for å følge skriftlige konvensjoner, f.eks. i titler eller for å markere starten på setninger.

## Hvordan gjøre det:
```Kotlin
fun main() {
    val originalText = "trondheim er fin"
    val capitalizedText = originalText.capitalize()
    println(capitalizedText) // Output: Trondheim er fin
}
```
I Kotlin kan `capitalize()` brukes for å kapitalisere kun første bokstav i strengen, men i nyere versjoner er den erstattet av `replaceFirstChar`.

```Kotlin
fun main() {
    val originalText = "trondheim er fin"
    val capitalizedText = originalText.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(capitalizedText) // Output: Trondheim er fin
}
```

## Dypdykk
Kapitalisering har gamle røtter i skriftspråket og er en standard måte å signalisere starten av en setning eller betydningen av et ord. I programmering speiler strengkapitalisering disse skrivereglene. Det finnes flere metoder for å kapitalisere strenger i Kotlin. `capitalize()` er enkel, men er faset ut i nyere versjoner til fordel for `replaceFirstChar` som gir mer kontroll og er kompatibel med Unicode-standarder, noe som er viktig når vi håndterer diverse skriftenheter på tvers av forskjellige språk.

En annen tilnærming er å bruke `toLowerCase()` kombinert med `toUpperCase()` for å kapitalisere hele strenger eller spesifikke deler:

```Kotlin
fun String.capitalizeFully(): String {
    return this.lowercase().split(" ").joinToString(" ") { it.replaceFirstChar { char -> 
        if (char.isLowerCase()) char.titlecase() else char.toString() 
    }}
}

fun main() {
    val originalText = "trondheim er FIN"
    val capitalizedText = originalText.capitalizeFully()
    println(capitalizedText) // Output: Trondheim Er Fin
}
```

## Se også
- [Kotlin Standard Library Functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- [Unicode Standard](http://unicode.org/standard/standard.html)
- [String.capitalize KDoc](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)