---
date: 2024-01-27 20:34:55.375036-07:00
description: "Generering av tilfeldige tall i programmering handler om \xE5 skape\
  \ tall som mangler ethvert forutsigbart m\xF8nster. Programmerere gj\xF8r dette\
  \ av ulike grunner,\u2026"
lastmod: '2024-03-13T22:44:40.746152-06:00'
model: gpt-4-0125-preview
summary: "Generering av tilfeldige tall i programmering handler om \xE5 skape tall\
  \ som mangler ethvert forutsigbart m\xF8nster. Programmerere gj\xF8r dette av ulike\
  \ grunner,\u2026"
title: Generering av tilfeldige tall
weight: 12
---

## Hva & Hvorfor?

Generering av tilfeldige tall i programmering handler om å skape tall som mangler ethvert forutsigbart mønster. Programmerere gjør dette av ulike grunner, inkludert simuleringer, algoritmetesting, spill og sikkerhetsapplikasjoner, hvor uforutsigbarhet er nøkkelen til å oppnå realistiske eller sikre resultater.

## Hvordan:

Kotlin gir en enkel måte å generere tilfeldige tall gjennom sitt standardbibliotek. Slik kan du generere forskjellige typer tilfeldige verdier:

### Generere et tilfeldig heltall

For å generere et tilfeldig heltall innenfor et spesifikt område:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Genererer et tilfeldig tall mellom 1 og 99
    println(randomNumber)
}
```

### Generere et tilfeldig desimaltall

På samme måte, for å generere et tilfeldig desimaltall:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Genererer et tilfeldig desimaltall mellom 1.0 og 10.0
    println(randomDouble)
}
```

### Generere en tilfeldig boolsk verdi

For å generere en tilfeldig boolsk verdi:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Genererer enten sant eller usant tilfeldig
    println(randomBoolean)
}
```

### Seed for reproduserbare resultater

I tilfeller hvor du trenger reproduserbare sekvenser av tilfeldige tall (for eksempel under testing), kan du seede generatoren av tilfeldige tall:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Dypdykk

Kotlin standardbiblioteks tilnærming til å generere tilfeldige tall benytter seg av Java sin `java.util.Random` under panseret, noe som sikrer en blanding av brukervennlighet og ytelse. Det er imidlertid viktig å merke seg at disse metodene genererer pseudotilfeldige tall, noe som betyr at tallene ser tilfeldige ut, men genereres ved hjelp av en deterministisk prosess.

For de fleste applikasjoner er tilfeldigheten levert av Kotlins `Random`-klasse tilstrekkelig. Imidlertid, for mer sikkerhetssensitive applikasjoner, som kryptografi, hvor kvaliteten på tilfeldigheten er av største viktighet, bør man vurdere å bruke `java.security.SecureRandom` i stedet. SecureRandom er spesielt designet for kryptografiske operasjoner, og tilbyr en høyere kvalitet på tilfeldigheten, men med en potensiell ytelsestradeav.

Kotlin finner ikke opp hjulet på nytt, men tilbyr et Kotlin-vennlig API over Javas mekanismer for generering av tilfeldige tall, noe som gjør det mer idiomatisk og kortfattet å bruke innen Kotlin-prosjekter. Som alltid, når man har å gjøre med tilfeldighet, bør programmerere nøye vurdere bruksområdet for å velge det mest passende verktøyet for jobben.
