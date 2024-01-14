---
title:    "Arduino: Generering av slumpmässiga nummer"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför:
Att generera slumpmässiga nummer är en viktig del av många Arduino projekt, oavsett om man bygger en leksak, en robot eller ett interaktivt spel. Slumpmässiga nummer ger variation och spänning till programmet och hjälper till att skapa spännande utfall. 

## Hur man gör:
För att generera slumpmässiga nummer på Arduino, används funktionen `random(min,max)` där "min" och "max" är de önskade intervallen för de slumpmässiga numren. Här är ett enkelt exempel på hur man kan använda denna funktion:

```Arduino
int tall1 = random(0,10); // Genererar ett slumpmässigt heltal mellan 0 och 10
float tall2 = random(0.0,5.5); // Genererar ett slumpmässigt decimaltal mellan 0.0 och 5.5
```

Det finns också möjlighet att generera slumpmässiga nummer baserat på en seed eller frövärde. Detta gör att man kan återskapa samma serie av slumpmässiga nummer, vilket kan vara användbart i spel eller simuleringar.

Genom att lägga till funktionen `randomSeed(seed)` innan genereringen av slumpmässiga nummer, kan man använda ett specifikt värde eller sensorvärde som frövärde.

```Arduino
randomSeed(analogRead(A0)); // Använder värdet från analog pin A0 som frövärde
int tall = random(0,100); // Genererar ett slumpmässigt tal mellan 0 och 100 baserat på frövärdet
```

## Djupdykning:
För att få en djupare förståelse för hur generering av slumpmässiga nummer fungerar på Arduino, är det viktigt att förstå att dessa nummer inte är helt slumpmässiga utan genereras baserat på matematiska algoritmer. Vissa algoritmer, som till exempel Linear Congruential Generator, kan vara mindre slumpmässiga och kan återskapa samma sekvenser av nummer.

För en mer tillförlitlig och slumpmässig generering av nummer, kan man istället använda sig av speciella bibliotek som erbjuder bättre algoritmer för slumpmässiga nummer. Ett sådant populärt bibliotek är `random_lib` som erbjuder flera olika funktioner för generering av nummer.

Genom att förstå grunderna för generering av slumpmässiga nummer och hur man kan använda olika algoritmer och frövärden, kan man skapa mer avancerade program och uppnå mer liknande slumpmässigheter.

## Se även:
- [Arduino Reference: random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino Tutorial: Random Numbers](https://www.arduino.cc/en/Tutorial/RandomNumbers)
- [Arduino Libraries: random_lib](https://www.arduino.cc/reference/en/libraries/random_lib/)