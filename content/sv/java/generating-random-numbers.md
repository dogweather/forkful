---
title:                "Generering av slumpmässiga siffror"
html_title:           "Java: Generering av slumpmässiga siffror"
simple_title:         "Generering av slumpmässiga siffror"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Random number generation, or slumptalsgenerering in Swedish, is an integral part of many programming tasks. Whether it's for creating unique identifiers, simulating games, or encryption algorithms, the ability to generate random numbers is essential for any programmer.

## Hur man gör
För att generera random numbers i Java, används metoden `random()` från klassen `Math`. Det tar inga argument och returnerar ett kommatal mellan 0.0 och 1.0. För att få ett heltal, multiplicera resultatet med det önskade antalet möjliga utfall och konvertera till ett heltal med `Math.round()`.

```Java
double random = Math.random(); //slumpmässigt kommatal mellan 0.0 och 1.0
int diceRoll = Math.round(random * 6); //slumptal mellan 1 och 6
```

Om du vill begränsa utfallen till ett specifikt intervall, lägg till en offset och använd `Math.floor()` för att avrunda neråt.

```Java
int randomRange = Math.floor(random * (max - min + 1) + min); //slumptal mellan min och max
```

## Deep Dive
Om du vill ha mer kontroll över slumptalsgenereringen, kan du använda klassen `Random` från paketet `java.util`. Den erbjuder fler metoder för att generera random numbers baserat på olika algoritmer, såsom `nextDouble()` för kommatal och `nextInt()` för heltal.

Det är också möjligt att specificera en "seed" för att få konsistenta resultat varje gång programmet körs. Detta kan vara användbart för testning och felsökning av kod som använder slumptal.

## Se även
- [Javadoc för klassen `Math` (Java 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--)
- [Javadoc för klassen `Random` (Java 8)](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)