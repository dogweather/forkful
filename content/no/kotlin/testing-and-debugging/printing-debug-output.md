---
title:                "Skrive ut feilsøkingsdata"
aliases: - /no/kotlin/printing-debug-output.md
date:                  2024-01-20T17:53:00.316880-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
**Hva & Hvorfor?**

Printing debug output lar oss vise verdier eller meldinger mens programmet kjører. Det er som en live-titt inn i appens hjerne, nyttig for å finne og fikse feil.

## How to:
**Slik gjør du:**

I Kotlin bruker vi ofte `println()` for å skrive ut til konsollen. Enkel og rett frem:

```Kotlin
fun main() {
    val bugFinding = "Edderkopp!"
    println("Debug: Fant en bug - $bugFinding")
}

// Output:
// Debug: Fant en bug - Edderkopp!
```

Vi kan også bruke logging-biblioteker for mer avanserte behov:

```Kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("MyAppLogger")
    logger.debug("Debugmelding: Sjekker variabelverdier.")
}

// Output vil variere avhengig av logger-konfigurasjon. 
```
  
## Deep Dive
**Dypdykk**

I gamle dager brukte vi `System.out.println()` i Java, men Kotlin kom og forenklet alt med `println()`. Andre alternativer inkluderer logging-biblioteker som Log4j, SLF4J eller Kotlin Logging. Logg-biblioteker tilbyr nivåer (info, debug, warning, error) og konfigurasjon for å styre hvor og hva som logges. Implementasjonsdetaljer varierer, men prinsippet er å gi en detaljert logg som hjelper ved debugging uten å blottlegge sensitiv informasjon.

## See Also
**Se også:**

- [Kotlin Logging documentation](https://github.com/MicroUtils/kotlin-logging)
- [SLF4J User Manual](http://www.slf4j.org/manual.html)
- [Baeldung on Logging in Kotlin](https://www.baeldung.com/kotlin/logging)
