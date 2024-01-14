---
title:    "Kotlin: Å generere tilfeldige tall"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall er en viktig del av mange programmeringsprosjekter. Det kan være nyttig for simuleringer, spill, sikkerhetsinfrastruktur, og mye mer.

## Hvordan
For å generere tilfeldige tall i Kotlin, kan vi bruke `Random()` funksjonen. Den vil generere et tall mellom 0 og 1 hver gang den blir kalt. For eksempel:

```Kotlin
val tilfeldigTall = Random()
println(tilfeldigTall.nextDouble())
// Utgang: 0.5323199247497136 
```

For å generere et tilfeldig heltall, kan vi bruke `nextInt()` funksjonen og gi den et argument for å spesifisere et øvre grense. For eksempel:

```Kotlin 
val tilfeldigHeltall = Random()
println(tilfeldigHeltall.nextInt(10))
// Utgang: 7 
```

For å generere flere tilfeldige tall, kan vi bruke en løkke og kalle `nextDouble()` eller `nextInt()` funksjonen hver gang.

## Dykk ned i detaljene
Det er viktig å merke seg at `Random()` funksjonen ikke genererer ekte tilfeldige tall, men følger en algoritme for å produsere en sekvens av tall som ser ut som tilfeldige. Dette betyr at tallene den genererer kan følge et mønster og ikke være helt tilfeldige. For de fleste tilfeller er dette godt nok, men hvis det er behov for ekte tilfeldighet, bør man vurdere å bruke annen type ekstern tilfeldighetsgenerator.

For å kontrollere hvilke tall `Random()` funksjonen genererer, kan vi spesifisere et såkalt "seed". Dette er et tall som brukes som utgangspunkt for algoritmen og vil resultere i samme sekvens av tall hver gang. For å gjøre dette, kan vi bruke `Random(seed)` konstruktøren i stedet for bare `Random()`.

## Se også
- [Official Kotlin documentation for Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Generating Random Numbers in Kotlin](https://kodejava.org/how-to-generate-random-numbers-in-kotlin/)