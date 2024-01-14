---
title:                "Kotlin: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er en viktig del av feilsøking og debugging i Kotlin-programmering. Ved å sende ut feilmeldinger til standard error i stedet for standard output, kan utviklere enkelt identifisere og løse potensielle problemer i koden sin.

## Hvordan

For å skrive til standard error i Kotlin, kan du bruke funksjonen `System.err.println()`. Dette vil skrive ut en melding til standard error-strømmen og få den til å vises i konsollen.

```Kotlin
fun main() {
    // Skriver ut en feilmelding
    System.err.println("Det har oppstått en feil!")

    // Skriver ut en variabel til standard error
    val num = 5
    System.err.println("Tallet er $num")
}
```

Outputen av dette vil være:

```
Det har oppstått en feil!
Tallet er 5
```

## Dypdykk

Når du skriver til standard error, er det viktig å huske på at eventuell tekst som blir skrevet ut vil vises som en feilmelding i konsollen, og ikke som standard output. Dette betyr at du bør være forsiktig med hva du skriver til standard error og kun bruke det for å rapportere feil eller viktige meldinger.

En annen viktig ting å huske på er at standard error-strømmen ikke blir påvirket av eventuelle endringer i standard output-strømmen. Dette betyr at uansett om du endrer innstillinger for standard output, vil feilmeldinger fortsatt bli skrevet til standard error.

## Se også

- [Offisiell Kotlin-dokumentasjon for standard error](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-print-stream/err.html)
- [Tutorial for feilsøking og debugging i Kotlin](https://kotlinlang.org/docs/tutorials/getting-started.html#debugging-classes-with-real-world-usage)