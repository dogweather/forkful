---
title:                "Generering av slumpmässiga tal"
html_title:           "Fish Shell: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga nummer är en vanlig uppgift för programmerare. Det kan användas för att skapa unika ID-nummer, simulerar spel eller testa program. Detta görs genom matematiska algoritmer som genererar slumpmässiga tal baserat på ett startvärde, kallat en frövärde.

## Hur gör man:
Fish Shell har inbyggda funktioner för att generera slumpmässiga nummer. Här är ett exempel på hur man genererar ett slumpmässigt heltal mellan 1 och 10:

```Fish Shell
set random_num (math --random 1 10)
echo $random_num
```
Det första kommandot sätter variabeln "random_num" till ett slumpmässigt tal mellan 1 och 10 med hjälp av den inbyggda matematikfunktionen "math --random". Sedan skrivs det slumpmässiga talet ut med hjälp av kommandot "echo".

## Djupdykning:
Generering av slumpmässiga nummer innebär matematiska algoritmer som tar ett startvärde och producerar en sekvens av nummer baserat på det. Det finns olika metoder för att skapa slumpmässiga tal och vissa är bättre än andra. En alternativ metod kallas "true random number generation" som hämtar tillfälliga värden från fysiska fenomen som radioaktivt sönderfall eller atmosfäriska brus. Fish Shell använder dock en metod som bygger på algoritmer för att skapa mer förutsägbara, men ändå slumpmässiga, tal.

## Se även:
Djupdykning: https://fishshell.com/docs/current/cmds/math.html#math---random