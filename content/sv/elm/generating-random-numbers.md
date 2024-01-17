---
title:                "Generera slumpmässiga nummer"
html_title:           "Elm: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga tal är en viktig del av programmering då det tillåter oss att skapa variation och oväntade resultat i våra program. Det är en vanlig teknik som används för att skapa spel, simuleringar och mycket mer.

## Hur to?
Elm har inbyggda funktioner för att hantera slumpmässiga tal, vilket gör det enkelt och effektivt att implementera i våra program. Nedan finns ett exempel på hur man genererar ett slumpmässigt tal mellan 1 och 10 i Elm:

```Elm
import Random

getNum : Int
getNum =
   Random.int 1 10
```

Detta kommer att returnera ett slumpmässigt heltal mellan 1 och 10 varje gång funktionen anropas. Om du vill generera ett slumpmässigt tal mellan två bestämda gränser kan du använda funktionen `Random.int` med önskade värden.

## Djupdykning
Den första implementationen av slumpmässiga tal i datorer var enkel och baserades på användning av fysiska processer, som exempelvis radioaktivt sönderfall. Idag finns det dock många olika sätt att generera slumpmässiga tal, varav vissa är mer effektiva och tillförlitliga än andra. Elm använder en algoritm som heter XorShift för att generera sina slumpmässiga tal, vilket är en populär metod inom programmering.

Det finns också alternativ till Elm för att skapa slumpmässiga tal, som exempelvis PureScript och Haskell. Dessa språk erbjuder också inbyggda funktioner för att hantera slumpmässiga tal och har liknande tillvägagångssätt som Elm.

## Se även
Vill du lära dig mer om hur slumpmässiga tal fungerar i Elm? Här är några användbara resurser: 

- [Elm's officiella dokumentation om Random Library][https://package.elm-lang.org/packages/elm/random/latest/]
- [En interaktiv tutorial om Elm][https://elmprogramming.com/]