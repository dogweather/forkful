---
title:                "Skriving til standardfeil"
html_title:           "Kotlin: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skriving til standardfeil er en måte for utviklere å uttrykke feil, advarsler og annen viktig informasjon under kjøring av et program. Dette er spesielt nyttig når du arbeider med store prosjekter, da det kan bidra til å identifisere potensielle problemer og feilsøke dem på en effektiv måte.

## Slik gjør du:

```Kotlin
fun main(args: Array<String>) {
  val age = 25
  println("Din alder er $age år.")
  System.err.println("Advarsel: Alder må være en positiv verdi.")
}
```
Output:
```
Din alder er 25 år.
Advarsel: Alder må være en positiv verdi.
```

## Dykk dypere:

Historisk sett, som ønsket å skrive til standardfeil, måtte utviklere ty til å bruke lavnivåfunksjoner og systemanrop. Med Kotlin kan vi enkelt bruke `System.err`-objektet til å skrive til standardfeil på en mer robust og plattform-uavhengig måte. Alternativt kan utviklere også bruke logging rammeverk som logback eller log4j for å ta vare på loggføring og skrive til standardfeil.

## Se også:

- Offisiell Kotlin dokumentasjon om `System.err`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-print-stream/err.html
- Mer informasjon om Logging: https://www.baeldung.com/kotlin-logging