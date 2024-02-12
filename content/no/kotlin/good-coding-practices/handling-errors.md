---
title:                "Feilhåndtering"
aliases: - /no/kotlin/handling-errors.md
date:                  2024-01-26T00:55:33.131050-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Håndtering av feil er hvordan koden din takler problemer som oppstår under utførelse—som å fange en kurveball uten å miste den. Programmerere gjør dette for å forhindre krasj og gi brukerne en jevn opplevelse.

## Hvordan:
Kotlin tilbyr `try`, `catch`, `finally`, og `throw` for å håndtere feil. Slik bruker du dem:

```Kotlin
fun main() {
    val teller = 10
    val nevner = 0

    try {
        val resultat = teller / nevner
        println("Resultat: $resultat")
    } catch (e: ArithmeticException) {
        println("Kan ikke dele på null, kompis.")
    } finally {
        println("Dette skjer uansett hva.")
    }
}
```

Output:
```
Kan ikke dele på null, kompis.
Dette skjer uansett hva.
```

Hvis noe går galt i `try`-blokken, går utførelsen straks til `catch`. Den fanger den spesifikke feilen som er kastet (`ArithmeticException` i dette tilfellet). `finally`-blokken kjøres etterpå—uavhengig av utfallet.

## Dypdykk
`try-catch`-blokken har vært en ting siden de tidlige programmeringsdagene—det er som et sikkerhetsnett. Kotlin tilbyr også `throw` for manuelt å kaste et unntak inn i ringen, og det er `finally` for kode som må kjøre—ofte oppryddingsarbeid.

Alternativer inkluderer `Result`-typen og Kotlins `try` som et uttrykk.

```Kotlin
val resultat: Result<Int> = try {
    Result.success(teller / nevner)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Denne tilnærmingen returnerer et `Result`-objekt—du får enten en suksess eller et feilslag uten dramaet med et ubehandlet unntak.

Implementering i Kotlin er ryddig fordi du kan bruke `try` som et uttrykk, noe som betyr at det returnerer en verdi. Valg som disse gjør feilhåndtering i Kotlin ganske allsidig. Det handler om å velge det riktige verktøyet for jobben, akkurat som du ville gjort i et verksted.

## Se også
- Kotlin-dokumentasjon om Unntak: [Kotlin Exception Handling](https://kotlinlang.org/docs/exception-handling.html)
- Kotlin `Result`-type dokumentasjon: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, 3. utgave, av Joshua Bloch—gode innsikter om unntak, selv om det er spesifikt for Java.
