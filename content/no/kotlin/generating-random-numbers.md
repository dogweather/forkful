---
title:                "Genererer tilfeldige tall"
html_title:           "PHP: Genererer tilfeldige tall"
simple_title:         "Genererer tilfeldige tall"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kort sagt, å generere tilfeldige tall er prosessen med å produsere nummersekvenser uten noen mønstersløyfe. Programmerere gjør dette for å legge til uforutsigbarhet i applikasjoner, og for å sikre dataintegritet i sikkerhetskryptering.

## Hvordan:

La oss dykke rett inn i koden.

For å lage et tilfeldig tall i Kotlin, kan du bruke `fun Random.nextInt(range: IntRange) : Int` funksjonen. 

```Kotlin
import kotlin.random.Random
fun main() {
   val random = Random.nextInt(0, 100)
   println("Tilfeldig tall: $random")
}
```

Når du kjører programmet, kan du få noe slikt: 

```
Tilfeldig tall: 66
```

## Dyp Dykk

Generering av tilfeldige tall har en rik historie som strekker seg tilbake til antikkens tider, og det er mange forskjellige algoritmer og teknikker for å utføre det. Kotlin har valgt å bruke pseudotilfeldige tallgeneratorer, som bruker matematiske formler eller forhåndsbestemte tabeller for å lage nummersekvenser som ser tilfeldige ut, men ikke egentlig er det.

Noen alternativer til `Random.nextInt` inkluderer `Random.nextFloat`, `Random.nextBoolean`, og mer. 

Ved nærmere øyekast på implementeringen av `Random` funksjonen, kan det være interessant å merke seg at under panseret bruker Kotlin java.util.Random klassen for tilfeldig tallgenerering.

## Se Også

For mer informasjon om dette emnet, kan du kikke på disse linkene:

1. [Kotlin Random Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
2. [Eric Lippert's series on Pseudorandom Number Generation](https://ericlippert.com/2013/12/16/how-much-bias-is-introduced-by-the-remainder-technique/)
3. [Java.util.Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)