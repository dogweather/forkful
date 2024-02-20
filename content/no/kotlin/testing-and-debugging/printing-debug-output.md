---
date: 2024-01-20 17:53:00.316880-07:00
description: "**Hva & Hvorfor?** Printing debug output lar oss vise verdier eller\
  \ meldinger mens programmet kj\xF8rer. Det er som en live-titt inn i appens hjerne,\
  \ nyttig\u2026"
lastmod: 2024-02-19 22:05:00.013774
model: gpt-4-1106-preview
summary: "**Hva & Hvorfor?** Printing debug output lar oss vise verdier eller meldinger\
  \ mens programmet kj\xF8rer. Det er som en live-titt inn i appens hjerne, nyttig\u2026"
title: "Skrive ut feils\xF8kingsdata"
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
