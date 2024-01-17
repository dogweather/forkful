---
title:                "Generering av tilfeldige tall"
html_title:           "Kotlin: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall er en vanlig praksis i programmering for å få tilfeldig data eller gjenskape tilfeldige situasjoner. Dette kan være nyttig for spill, sikkerhetsfunksjoner, eller testing av kode.

## Hvordan:
```Kotlin
//Eksempel på generering av tilfeldig tall innenfor et bestemt område
val randomNum = (1..10).random()
print(randomNum) // Output: tilfeldig tall mellom 1 og 10

//Eksempel på generering av tilfeldig tall fra en liste av verdier
val options = listOf("hund", "katt", "fisk", "fugl")
val randomChoice = options.random()
print(randomChoice) // Output: en tilfeldig valgt verdi fra listen

//Eksempel på generering av flere tilfeldige tall
val randomList = (1..7).map { (1..10).random() }
print(randomList) // Output: en liste med syv tilfeldige tall mellom 1 og 10
```

## Dypdykk:
Generering av tilfeldige tall har vært en utfordring i dataprogrammering på grunn av dets avhengighet av deterministiske algoritmer. Tidligere, var pseudorandom tallgenerering (PRNG) den vanligste metoden, hvor en algoritme bruker en startverdi for å produsere tall som kan virke tilfeldige, men er egentlig forutsigbare. I Kotlin, er det flere alternativer for tilfeldig tallgenerering, som inkluderer bruk av PRNG, eksterne APIer eller maskinvarebaserte tilfeldig tallgeneratorer.

## Se også:
- [Offisiell Kotlin dokumentasjon for tilfeldig tallgenerering](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-r-a-n-d-o-m/)
- [Artikkel om tilfeldig tallgenerering og sikkerhet i Kotlin](https://blog.jetbrains.com/kotlin/2019/04/security-and-randomness/)