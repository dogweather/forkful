---
title:                "Java: Generera slumpmässiga nummer"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

Varför: Att använda slumpmässiga nummer i Java-programmering kan vara användbart för att skapa spel eller simuleringar som kräver variation och slumpmässighet.

## Hur man gör

```java
// Importera Random-klassen
import java.util.Random;

// Skapa ett objekt av Random-klassen
Random rand = new Random();

// Generera ett slumpmässigt heltal mellan 1 och 100
int randomNumber = rand.nextInt(100) + 1;

// Generera ett slumpmässigt flyttal mellan 0 och 1
double randomDouble = rand.nextDouble();

// Använda slumpmässiga nummer i en loop
for (int i = 0; i < 10; i++) {
  int randNum = rand.nextInt(10) + 1;
  System.out.println("Slumpmässigt nummer: " + randNum);
}
```

Exempelutgång:

```
Slumpmässigt nummer: 6
Slumpmässigt nummer: 3
Slumpmässigt nummer: 9
Slumpmässigt nummer: 10
Slumpmässigt nummer: 2
Slumpmässigt nummer: 1
Slumpmässigt nummer: 5
Slumpmässigt nummer: 4
Slumpmässigt nummer: 10
Slumpmässigt nummer: 8
```

## Djupgående information

I Java finns det två sätt att generera slumpmässiga nummer - `Random` -klassen och `Math.random()`-metoden.

`Random`-klassen har flera metoder för att generera slumpmässiga nummer av olika datatyper såsom `nextInt()` och `nextDouble()`. Det är viktigt att skapa ett objekt av `Random`-klassen innan man kan använda dess metoder.

`Math.random()`-metoden genererar en slumpmässig flyttal mellan 0 och 1. Man kan sedan använda matematiska operationer för att begränsa området eller omvandla det till en annan datatyp.

Slumpmässiga nummer genereras baserat på ett så kallat "seed", som är ett startvärde för den generator som används. Om man inte anger en seed så används datorsystemets tid som seed, vilket resulterar i en annan sekvens av slumpmässiga nummer varje gång programmet körs.

## Se även

- [Random-klassen i Java API](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html)
- [Math-klassen i Java API](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/Math.html#random())