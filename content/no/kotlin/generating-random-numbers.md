---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:31.757095-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å generere tilfeldige tall i programmering betyr å skape et nummer som er uforutsigbart. Programmerere bruker tilfeldige tall for alt fra spillmekanikk til å teste algoritmer og sikkerhetssystemer.

## Slik gjør du:
```kotlin
import kotlin.random.Random

fun main() {
    // Enkelt tilfeldig heltall mellom 0 (inkludert) og 100 (ekskludert)
    val randomInt = Random.nextInt(100)
    println(randomInt)
    
    // Tilfeldig tall med desimaler (Double) mellom 0.0 (inkludert) og 1.0 (ekskludert)
    val randomDouble = Random.nextDouble()
    println(randomDouble)
    
    // Tilfeldig element fra en liste
    val colors = listOf("Rød", "Grønn", "Blå")
    val randomColor = colors.random()
    println(randomColor)
}
```

### Eksempel på utdata:
```
42
0.1234567890123456
Grønn
```

## Dypdykk
Tilfeldige tall har vært sentrale i informatikkens historie - fra simuleringer og kryptografi til kunstig intelligens. I eldre programmeringsspråk, som C, måtte man ofte initialisere en tilfeldighetsgenerator med systemklokken for å få varierte resultater. Kotlin forenkler prosessen ved å tilby innebygde funksjoner gjennom `Random` klassen. Alternativene til `kotlin.random.Random` inkluderer bruk av Java's `ThreadLocalRandom` for trådsikre operasjoner, eller `SecureRandom` for kryptografisk sikker tilfeldighet. For detaljer om implementasjon: `Random` bruker en bestemt algoritme for å produsere en sekvens av tall som virker tilfeldige, men med samme startpunkt (seed), produserer de samme nummersekvensene.

## Se også
- [Kotlin-dokumentasjon for Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Oracle Java-dokumentasjon for SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [Wikipedia-artikkel om pseudotilfeldighetss-generatorer](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
