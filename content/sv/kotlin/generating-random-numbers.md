---
title:                "Kotlin: Generering av slumpmässiga tal"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Varför
Generering av slumpmässiga tal är en viktig del av programmering när man behöver skapa olika spel och simuleringar, eller när man behöver testa kod som är beroende av slumpmässighet.

##Hur man gör
För att generera slumpmässiga tal i Kotlin använder man funktionen `random()` från standardbiblioteket `kotlin.random`.

```Kotlin
// Importera biblioteket
import kotlin.random.Random

// Generera ett slumpmässigt heltal mellan 1 och 10
val randomNumber = Random.nextInt(1, 11)

// Generera ett slumpmässigt decimaltal mellan 0 och 1
val randomDecimal = Random.nextDouble()
```

Man kan också ange ett seed-värde för att få samma slumpmässiga tal varje gång man kör koden. Det kan vara användbart för testning eller när man vill återskapa en specifik sekvens av slumpmässiga tal.

```Kotlin
// Generera ett slumpmässigt heltal mellan 1 och 10 med seed-värde 42
val randomNumberWithSeed = Random(42).nextInt(1, 11)
```

Om man vill ha ett slumpmässigt element från en lista eller array kan man använda funktionen `random()` och ange det som en gräns i `nextInt()` funktionen.

```Kotlin
val names = listOf("Anna", "Erik", "Maria", "Lisa")

// Välja ett slumpmässigt namn från listan
val randomName = names[Random.nextInt(names.size)]
```

##Djupdykning
För att förstå hur funktionen `random()` fungerar bakom kulisserna kan man titta på dess definition i Kotlin:s källkod. Den använder sig av en pseudo-slumpmässig sekvensgenerator baserad på en algoritm som kallas "Xorshift".

Algoritmen använder sig av ett startvärde och genererar sedan en sekvens av tal baserat på detta värde, där varje nytt tal använder det föregående talet för att generera nästa.

Det är också viktigt att komma ihåg att dessa slumpmässiga tal egentligen inte är helt slumpmässiga, utan baseras på ett startvärde och en bestämd sekvens. Om man behöver verkligt slumpmässiga tal för till exempel kryptering, bör man istället använda en dedikerad sekvensgenerator för slumpmässighet.

##Se även
- [Kotlin Random API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/)
- [Pseudo-random number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Xorshift algorithm](https://en.wikipedia.org/wiki/Xorshift)