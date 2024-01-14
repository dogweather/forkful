---
title:    "Java: Generering av slumpmässiga tal"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför du bör använda dig av slumpmässiga nummer

Att kunna generera slumpmässiga nummer kan vara en ovärderlig färdighet för programmerare och kan ha många olika användningsområden. Det kan användas för att testa program, skapa olika spelupplevelser eller till och med för kryptografiska ändamål. I denna bloggpost kommer vi att gå igenom hur man genererar slumpmässiga nummer i Java och vad som ligger bakom denna funktion.

## Så här gör du

För att generera slumpmässiga nummer i Java kan vi använda klassen `Random`. Det första steget är att importera detta objekt:

```Java
import java.util.Random;
```

Sedan kan vi skapa ett objekt av typen `Random` och använda dess metoder för att generera olika typer av nummer. Till exempel kan vi använda `nextInt()` för att generera ett heltalsvärde mellan 0 och det specificerade övre gränsvärdet. Här är ett exempel på kod som genererar och skriver ut 10 heltal mellan 1 och 100:

```Java
Random rand = new Random();

for (int i = 0; i < 10; i++) {
    int number = rand.nextInt(100) + 1;
    System.out.println(number);
}
```

Detta kommer att ge oss 10 slumpmässiga heltal, där det högsta värdet är 100 och det lägsta är 1. Om vi istället vill ha decimaltal mellan 0 och 1 kan vi använda `nextDouble()`:

```Java
double decimal = rand.nextDouble();
System.out.println(decimal);
```

Det finns också andra metoder som `nextBoolean()` för att generera slumpmässiga booleska värden, `nextFloat()` för decimaltal mellan 0 och 1 och `nextLong()` för stora heltal. Du kan utforska dessa och fler metoder genom att kolla in Java-dokumentationen för klassen `Random`.

## Djupdykning

Nu när vi har sett hur man kan använda `Random`-klassen för att generera slumpmässiga nummer, låt oss titta på vad som faktiskt händer bakom kulisserna. `Random`-klassen använder sig av en algoritm för att producera slumpmässiga tal, som kallas en pseudo-slumpgenerator. Detta innebär att resultaten som genereras inte är helt slumpmässiga, utan baseras på ett startvärde som kallas en seed. Seed-värdet kan antingen sättas manuellt vid skapandet av `Random`-objektet eller så används en standard seed baserad på systemets aktuella tid.

Varje gång en metod som `nextInt()` anropas så ändras seed-värdet och nästa tal som genereras blir därför annorlunda. Detta är anledningen till att vi får olika resultat varje gång vi kör vårt program.

Det är viktigt att notera att denna algoritm inte är perfekt och det finns en teoretisk möjlighet för att vissa tal kan upprepas mer frekvent än andra. Om du letar efter en mer pålitlig och säker metod för att generera slumpmässiga tal, bör du istället använda dig av klassen `SecureRandom`.

## Se även

- Java-dokumentation för klassen `Random`: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- Artikel om upprepning av tal i `Random`-klassen: https://medium.com/@sapanzaveri/avoid-random-values-repetition-in-java-random-classes-e-phasesing-6bacc0a3f2b2
- Java-dokumentation för klassen `SecureRandom`: https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html