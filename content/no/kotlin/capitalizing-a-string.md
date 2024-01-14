---
title:    "Kotlin: Store bokstaver i en tekststreng"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi se nærmere på hvordan du kan kapitalisere en streng i Kotlin-programmeringsspråket. Dette kan være nyttig for å endre utseendet til en tekst eller for å følge standardiserte formateringsregler.

## Slik gjør du det

For å kapitalisere en streng i Kotlin, må du først sørge for at du har importert klassen `java.util.*` i filen din. Deretter kan du bruke funksjonen `toUpperCase()` på strengen du ønsker å gjøre stor og `toLowerCase()` for å gjøre den liten.

```Kotlin
import java.util.*

fun main() {
    val tekst = "dette er en tekst som skal gjøres stor"
    val storTekst = tekst.toUpperCase()
    val litenTekst = tekst.toLowerCase()

    println(storTekst) // Dette er en tekst som skal gjøres stor
    println(litenTekst) // dette er en tekst som skal gjøres liten
}
```

Som du kan se i eksempelet over, har vi laget en variabel `tekst` som lagrer en frase i små bokstaver. Deretter har vi brukt funksjonene `toUpperCase()` og `toLowerCase()` på `tekst`-variabelen for å endre bokstavene til henholdsvis store og små. Til slutt skriver vi ut de nye variantene av teksten ved hjelp av `println()`.

## Dypdykk

Når vi bruker `toUpperCase()` og `toLowerCase()`-funksjonene, blir strengen behandlet som en `String`-objekt i Kotlin. Dette betyr at funksjonene kan brukes på alle typer strenger, uavhengig av om de er av typen `var` eller `val`.

Det finnes også flere muligheter for å kapitalisere en tekst i Kotlin, som for eksempel å bruke funksjonen `capitalize()` som gjør om den første bokstaven i en streng til stor bokstav, og resten av bokstavene til små.

En annen nyttig funksjon er `replace()`, som lar deg erstatte deler av en streng med en annen streng. Dette kan være nyttig hvis du ønsker å endre utseendet på en bestemt del av teksten eller rette skrivefeil.

## Se også

For mer informasjon om håndtering av strenger i Kotlin, kan du sjekke ut følgende ressurser:

- [Kotlins offisielle dokumentasjon om strenger](https://kotlinlang.org/docs/reference/strings.html)
- [Stack Overflow-svar på hvordan man kan kapitalisere en streng i Kotlin](https://stackoverflow.com/questions/43285811/how-to-capitalize-the-first-letter-of-a-string-in-kotlin)
- [En tutorial om å formatere tekst i Kotlin](https://medium.com/@bobobdk/formatting-strings-like-a-champ-in-kotlin-fc9bff4e4bb2)