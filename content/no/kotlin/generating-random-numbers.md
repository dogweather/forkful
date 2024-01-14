---
title:                "Kotlin: Generering av tilfeldige tall."
simple_title:         "Generering av tilfeldige tall."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor
Å generere tilfeldige tall er en vanlig oppgave i mange programmeringsoppgaver. Dette kan være nyttig for å lage spill, utføre tester eller simulere ulike situasjoner. I Kotlin, kan du enkelt generere tilfeldige tall ved hjelp av innebygde funksjoner.

# Hvordan
Det finnes flere måter å generere tilfeldige tall på i Kotlin. En av de enkleste er å bruke Random-klassen. Denne klassen har en rekke metoder for å generere forskjellige typer tilfeldige tall.

```Kotlin
// Importer Random-klassen
import java.util.Random

// Hent en instans av Random-klassen
val random = Random()

// Generer et tilfeldig heltall mellom 1 og 100
val randomNumber = random.nextInt(100) + 1

// Generer et tilfeldig desimaltall mellom 0.0 og 1.0
val randomDouble = random.nextDouble()

// Generer et tilfeldig boolean-verdi
val randomBoolean = random.nextBoolean()
```

Her bruker vi først ```import```-uttrykket for å få tilgang til Random-klassen. Deretter oppretter vi en instans av klassen og bruker dens metoder for å generere tilfeldige tall. Merk at metoden ```nextInt(int bound)``` tar et argument som setter den øvre grensen for det tilfeldige tallet vi ønsker å generere.

# Dypdykk
Hvis du ønsker å kunne kontrollere hvordan tilfeldige tall blir generert, kan du også bruke Random-klassens konstruktører. I disse kan du angi en såkalt "seed" som bestemmer hvilken sekvens av tall som blir generert.

```Kotlin
// Opprett en instans av Random-klassen med en seed på 100
val random = Random(100)

// Generer et tilfeldig heltall mellom 1 og 100
val randomNumber = random.nextInt(100) + 1

// Generer et tilfeldig desimaltall mellom 0.0 og 1.0
val randomDouble = random.nextDouble()

// Generer et tilfeldig boolean-verdi
val randomBoolean = random.nextBoolean()
```

Ved å bruke samme seed vil du alltid få samme sekvens av tilfeldige tall. Dette kan være nyttig for testing og debugging.

# Se også
- [Random-klassen i Kotlin docs](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Generering av tilfeldige tall i Java](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
- [Tilfeldig tallgenerator eksempelprosjekt i Kotlin](https://github.com/Kotlin/kotlin-examples/tree/master/examples/random-numbers)