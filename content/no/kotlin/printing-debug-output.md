---
title:                "Kotlin: Utskrift av feilsøkingsresultater"
simple_title:         "Utskrift av feilsøkingsresultater"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor?

Feilsøking er en viktig del av programmeringsprosessen uansett hvilket språk du bruker. Ved å skrive ut debug-utgang kan du identifisere og løse problemer i koden din. Selv om dette kan virke som en enkel oppgave, er det viktig å forstå hvorfor det er nyttig å gjøre det riktig.

## Hvordan gjør man det?

Når det kommer til Kotlin, er det flere måter å skrive ut debug-utgang på. En måte er å bruke `println()` funksjonen, som vil skrive ut dataen til standardutgangen. For å gjøre dette, bruker du bare `println()` etterfulgt av variabelen du vil skrive ut, som vist i eksempelet nedenfor:

```Kotlin
val nummer = 42
println(nummer)
```

Output: 42

En annen måte å skrive ut debug-utgang på er å bruke `$`-tegnet for å interpolere variabler inn i en streng, som vist i eksempelet nedenfor:

```Kotlin
val navn = "Sara"
println("Mitt navn er $navn")
```

Output: Mitt navn er Sara

For mer komplisert debugging, kan du også bruke `debug`-funksjonen fra Kotlin's standard bibliotek. Dette vil gi deg mer detaljert informasjon om verdien av en variabel, da det konverterer den til en streng og skriver den ut. Bruk av `debug`-funksjonen krever en ekstra import og ser slik ut:

```Kotlin
import kotlin.text.debug

val navn = "Sara"
debug(navn)
```

Output: "Sara"

## Dykk dypere ned

Nå som du vet hvordan du skriver ut debug-utgang i Kotlin, er det viktig å forstå når og hvor du skal bruke det. Det er spesielt nyttig når du jobber med store og komplekse koder, da det kan hjelpe deg med å identifisere hvilken del av koden som forårsaker feil. Det kan også være nyttig når du vil sjekke verdien av variabler i ulike deler av koden for å sikre at de er riktig satt til det du forventer.

Det er viktig å huske å fjerne all debug-utgang før du publiserer eller deler koden din. Dette vil sikre at sluttbrukere ikke ser eller påvirkes av debug-beskjeder.

## Se også

- [Offisiell Kotlin dokumentasjon](https://kotlinlang.org/docs/tutorials/kotlin-for-py/debugging.html)
- [Debugging i Kotlin av CodeBrainer](https://www.youtube.com/watch?v=9nL3wpKjDrg)
- [Feilsøking og logging i Kotlin av Ray Wenderlich](https://www.raywenderlich.com/13909341-debugging-and-logging-in-kotlin)