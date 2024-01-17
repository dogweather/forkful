---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga nummer är en vanlig uppgift för programmerare, särskilt inom spel- och kryptografindustrin. Detta är ett sätt att skapa variation och osäkerhet i program och spel, vilket leder till en mer realistisk och spännande upplevelse för användaren.

## Så här gör du:
För att generera slumpmässiga nummer i Arduino kan du använda funktionen random(), som är inbyggd i Arduino-miljön. Detta gör det enkelt att skapa ett nummer inom ett specificerat intervall. Koden nedan visar hur du kan använda funktionen:

```Arduino 
int randomNumber = random(1, 10);
Serial.println(randomNumber);
```
Detta kommer att skriva ut ett slumpmässigt nummer mellan 1 och 10 i serieporten.

## Djupdykning:
För att förstå hur generering av slumpmässiga nummer fungerar, kan det vara intressant att titta på lite historik. Innan datorer hade utvecklat algoritmer för att skapa slumpmässiga nummer, användes fysiska metoder som tärningar och kortlekar för att skapa osäkerhet i spel.

Det finns också alternativa sätt att generera slumpmässiga nummer i Arduino, som att använda en extern klocka eller störningar i kretsen för att skapa ett osäkert nummer. Detta kan vara användbart för mer avancerade krypteringsapplikationer.

När det gäller implementationen av random() i Arduino, baseras den på en algoritm som kallas "linear congruential generator". Detta är en enkel metod för att skapa ett pseudoslumpmässigt tal baserat på en startpunkt och ett särskilt multiplikator.

## Se även:
Om du vill lära dig mer om slumpmässiga nummer och genereringen av dem, kan du ta en titt på följande länkar:

- [Arduino reference för random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [En grundläggande förklaring av algoritmer för generering av slumpmässiga nummer](https://www.geeksforgeeks.org/random-number-generator-in-arbitary-probability-distribution-fashion/)
- [En översikt av datorers historia och utvecklingen av slumpmässiga nummer](https://history-computer.com/ModernComputer/Basis/random.html)