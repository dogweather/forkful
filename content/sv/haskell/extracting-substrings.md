---
title:                "Att extrahera substrängar"
html_title:           "Haskell: Att extrahera substrängar"
simple_title:         "Att extrahera substrängar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar är en process där man plockar ut en del av en sträng och använder den för ett specifikt syfte, som att hitta en viss del av information. Det är en vanlig teknik som används av programmerare för att manipulera data på ett effektivt sätt.

## Så här gör man:
För att extrahera substrängar i Haskell kan man använda funktionen `take` och `drop`. Dessa funktioner tar in en sträng och returnerar en ny sträng med de specifierade delarna borttagna eller bevarade. Här är ett exempel på hur man kan använda dem:

```Haskell
-- Extraherar första tre tecknen från strängen "Hej världen"
take 3 "Hej världen" --> "Hej"

-- Tar bort första tre tecknen från strängen "Hej världen"
drop 3 "Hej världen" --> "världen"
```

## Djupdykning:
Extrahering av substrängar har funnits sedan tidigare programmeringsspråk som C och Pascal. I dessa språk används vanligtvis funktioner som `substr` och `substring`. I Haskell är metoderna `take` och `drop` mer fördelaktiga eftersom de är rekursiva och inte kopierar hela strängen varje gång de anropas.

Närliggande metoder för att hantera substrängar inkluderar `splitAt` som delar en sträng i två delar, `subsequences` som genererar alla möjliga delsträngar och `isPrefixOf` som kontrollerar om en sträng är en del av en annan.

## Se även:
- [Haskell documentation för `take` och `drop`](https://www.haskell.org/onlinereport/standard-prelude.html#function-selecting-list-elements)
- [Complete Haskell tutorial](https://wiki.haskell.org/Haskell_in_5_steps)
- [Online Haskell compiler](https://www.tutorialspoint.com/execute_haskell_online.php)