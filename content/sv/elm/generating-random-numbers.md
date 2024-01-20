---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generera Slumpmässiga Siffror med Elm

## Vad & Varför?

Att generera slumpmässiga siffror handlar om att skapa siffror utan något uppenbart mönster. Utan förutsägbarhet blir siffror effektiva i allt från att driva spellogik till att göra säkerhetskryptering starkare.

## Så här gör du:

Här är ett exempel på hur du genererar en enkel slumpmässig siffra i Elm.

```Elm
import Random

main =
    Random.generate identity (Random.int 1 100)
```

Vår kod genererar en slumpmässig heltal mellan 1 och 100 när det körs.

## Djup Dykning

Historiskt sett har slumpmässiga tal används sedan antikens tid, i spel och förutsägelser. Inom datavetenskap används det på olika sätt, från statistisk simulering till maskininlärning och spelutveckling.

Elm använder en pseudoslumpmässig talgenerator. Det betyder att numren verkar vara slumpmässiga, men om du har samma startpunkt, eller "frö", kommer de att producera samma sekvens varje gång. Detta kan vara användbart för att testa och felsöka din kod.

Ett alternativ till `Random.int` är `Random.float`, som genererar ett slumpmässigt flyttal mellan de två värden du ger det.

## Se Även

För mer detaljerad information om `Random` modulen, och för ytterligare funktioner för att generera andra typer av slumpmässiga värden, besök Elm dokumentation här [Elm Random Documentation](https://package.elm-lang.org/packages/elm/random/latest/Random). 

För att fullt ut förstå konceptet med "frö" och hur det påverkar sekvensen av slumpmässiga tal, kan denna artikel vara användbar [What is a Random Seed?](https://www.statisticshowto.com/random-seed-definition/).