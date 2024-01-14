---
title:                "Kotlin: Utskrift av feilsøkingsutdata"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være utfordrende å finne feil i koden din. Du kan tilbringe timer med å skrive og teste koden din, men likevel ikke være sikker på hvor feilen ligger. Det er her utskrift av feilsøkingsutdata kommer inn i bildet. Ved å skrive ut variabler, verdier og kodelinjer kan du enklere få oversikt over hva som skjer i koden din og finne feilen raskere.

## Hvordan

Det er flere måter å skrive ut feilsøkingsutdata i Kotlin, men den enkleste er å bruke funksjonen `println()`. Denne funksjonen tar inn en hvilken som helst datatype som argument og skriver den ut på skjermen. Her er et eksempel på hvordan du kan skrive ut en variabel og en streng i Kotlin:

```Kotlin
var nummer = 42
println("Verdien av variabelen nummer er: $nummer")
```

Output: Verdien av variabelen nummer er: 42

Du kan også bruke en tekstmaler for å lage en mer beskrivende melding:

```Kotlin
var navn = "Kari"
var alder = 30
println("$navn er $alder år gammel.")
```

Output: Kari er 30 år gammel.

Husk at du kan skrive ut så mange variabler og verdier du trenger for å få en god oversikt over koden din.

## Dypdykk

Når du bruker `println()` for feilsøking, er det viktig å være klar over at denne funksjonen kun skal brukes midlertidig. Å ha mange utskrifter i koden din kan senke ytelsen og gjøre koden vanskeligere å lese. Derfor bør du fjerne utskriftene når du har funnet og løst feilen.

En annen måte å skrive ut feilsøkingsutdata på er ved å bruke loggfunksjoner som `Log.d()` eller `Log.e()` i Android-utvikling. Disse funksjonene er nyttige i større prosjekter og gjør det mulig å filtrere utskriftene dine basert på nivå (debug, error, osv.) og komponent i koden.

## Se Også

- [Offisiell dokumentasjon om feilsøking i Kotlin] (https://kotlinlang.org/docs/reflection.html#debugging)
- [En guide til feilsøking i Kotlin] (https://www.raywenderlich.com/6013-kotlin-debugging-on-android-for-beginners)
- [Eksempelkode med utskrift av feilsøkingsutdata] (https://github.com/kotlin/kotlinx.serialozation/blob/master/docs/troubleshooting.md#printing-debug-output)