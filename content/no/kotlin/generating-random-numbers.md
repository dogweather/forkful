---
title:                "Kotlin: Generering av tilfeldige tall"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Tilfeldige tall er en viktig del av mange programmeringsprosjekter, spesielt når det kommer til å lage spill, simuleringer og generering av testdata. Å kunne generere tilfeldige tall kan være essensielt for å gjøre koden din mer dynamisk og realistisk. I denne bloggposten vil vi utforske hvordan du kan implementere tilfeldige tall i programmene dine ved hjelp av Kotlin.

## Slik
Det er flere måter å generere tilfeldige tall i Kotlin på, men den enkleste måten er å bruke `Random()` funksjonen. Dette vil generere en tilfeldig tallserie basert på en algoritme.

```Kotlin
val tilfeldigTall = Random()
println(tilfeldigTall.nextInt(100)) // Output: et tilfeldig tall mellom 0 og 99
println(tilfeldigTall.nextDouble()) // Output: et tilfeldig tall mellom 0.0 og 1.0
```

Vi kan også begrense nummerområdet til å passe vårt behov ved å angi en start og sluttverdi for `nextInt()` funksjonen.

```Kotlin
println(tilfeldigTall.nextInt(10, 20)) // Output: et tilfeldig tall mellom 10 og 20
```

Vi kan også bruke `nextInt()` funksjonen til å generere negative tall.

```Kotlin
println(tilfeldigTall.nextInt(-10, 10)) // Output: et tilfeldig tall mellom -10 og 10
```

For å få en større grad av kontroll, kan vi bruke `Random` konstruktøren sammen med en `Seed` for å generere en bestemt tallsekvens.

```Kotlin
val seed = 123
val tilfeldighet = Random(seed)
println(tilfeldighet.nextInt()) // Output: -1156286128
println(tilfeldighet.nextDouble()) // Output: 0.0657353604817802
```

Vi kan også bruke Kotlin's `Random` biblioteket for å generere tilfeldige bokstaver og tegn.

```Kotlin
println(tilfeldighet.nextBoolean()) // Output: false
println(tilfeldighet.nextChar()) // Output: 0
```

Disse er bare noen få eksempler på hvordan vi kan bruke `Random` funksjonen i Kotlin for å generere tilfeldige tall. Ved å eksperimentere med forskjellige funksjoner og parametere, kan du skape enda mer komplekse tilfeldige tallsekvenser.

## Dypdykk
Å generere tilfeldige tall kan virke som en enkel oppgave, men det er viktig å forstå at disse tallene faktisk ikke er 100% tilfeldige. De blir generert av en algoritme, og derfor er de ikke helt tilfeldige. Det kan også være forskjeller mellom hvordan forskjellige programmeringsspråk genererer tilfeldige tall, så det er viktig å velge en pålitelig og effektiv metode når du bruker tilfeldige tall i prosjektene dine.

For å komme nærmere ekte tilfeldighet, kan du også bruke ekte tilfeldige tallgeneratorer som fysisk måler naturlige fenomener, som radioaktivt henfall eller atmosfæriske støy. Disse tallene er mye mer tilfeldige enn de som genereres av en algoritme.

## Se også
- [Offisiell Kotlin dokumentasjon om Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Java's Random vs. SecureRandom - What is the Difference?](https://www.baeldung.com/java-random-securerandom)
- [The Problem with Using Random Numbers to Generate Game Content](https://www.gamasutra.com/blogs/MichaelBridges/20180122/309909/The_Problem_with_Using_Random_Numbers_to_Generate_Game_Content.php)