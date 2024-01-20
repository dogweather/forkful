---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skrive ut debuggingsdata er prosessen hvor programmerere logger viktige data og informasjon i koden for feilsøking. Det hjelper oss å spore feilene for å rette dem lettere.

## Hvordan gjøre det:

Her er en grunnleggende måte å skrive ut debuggingsdata på Kotlin.

```Kotlin
fun main() {
    val x = 5
    val y = 0
    try {
        val z = x / y
    } catch(e: Exception) {
        println("Feil: ${e.message}")
    }
}
```

Når vi kjører denne koden, vil vi se følgende utskrift:

```
Feil: / by zero
```

Vi bruker her `println` funksjonen til å logge feilen.

## Dypdykk 

Å skrive ut debuggingsdata har lang historie. Tidlige programmerere brukte ofte lyspaneler og skrivere for å spore programflyten. I våre dager blir debuggingsdata skrevet til konsollen eller loggfilene.

Som et alternativ til `println`, kan du bruke logging biblioteker som Log4j eller SLF4J, som gir mer avanserte logging funksjoner. 

En detalj er at `println` i Kotlin faktisk kaller System.out.println i Java, noe som betyr at det kan være låsekostnader når mange tråder prøver å skrive til konsollen samtidig.

## Se Også

- Dokumentasjon for Kotlin logging bibliotek: [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging)
- Offisiell Kotlin dokumentasjon: [println funksjon](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/println.html)