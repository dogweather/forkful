---
title:    "Haskell: Extrahering av delsträngar"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I Haskell-programmering finns det olika metoder för att hantera textsträngar. En av de vanligaste är att extrahera delsträngar, vilket innebär att ta en del av en befintlig sträng och använda den på olika sätt. Detta är användbart för att göra manipulationer eller sökningar i en textsträng. I denna bloggpost kommer vi att utforska hur man kan extrahera delsträngar i Haskell.

## Så här gör du

För att extrahera delsträngar i Haskell använder vi funktionen "take" och "drop" från språkets standardbibliotek. Dessa funktioner tar in en integervariabel och en sträng som argument och returnerar antingen början eller slutet av strängen, beroende på vilken funktion som används.

För att ta en del av en sträng kan vi använda funktionen "take" tillsammans med ett positivt tal som anger hur många tecken vi vill ta. Till exempel, om vi vill ta de första fyra tecknen i strängen "Hej" kan vi använda följande kod:

```Haskell
take 4 "Hej"
```

Detta skulle resultera i en ny sträng "Hej".

På samma sätt kan vi använda funktionen "drop" för att ta bort en del av en sträng. Till exempel, om vi vill ta bort de två första tecknen i strängen "Hej" kan vi använda följande kod:

```Haskell
drop 2 "Hej"
```

Detta skulle resultera i en ny sträng "j".

Vi kan också kombinera funktionerna "take" och "drop" för att få ett specifikt intervall av tecken från en sträng. Till exempel, om vi vill ta de tre mittersta tecknen i strängen "katten" kan vi använda följande kod:

```Haskell
(take 3 (drop 1 "katten"))
```

Detta skulle ge oss strängen "att".

## Djupdykning

För mer komplicerade fall av extrahering av delsträngar i Haskell, kan vi använda funktionen "splitAt" som tar in två integervariabler och en sträng som argument. Den delar strängen på positionen som anges av det första talet och returnerar två nya strängar som resultat.

Vi kan också använda funktionen "subSeq" från paketet "text", som tar in start- och slutindex och en sträng som argument och returnerar en delsträng mellan dessa index.

## Se även

- [Haskell Wiki: Strings](https://wiki.haskell.org/Strings)
- [Haskell String tutorial](https://thehaskelltutorial.com/string/)
- [Learn You a Haskell for Great Good! - Strings](http://learnyouahaskell.com/starting-out#ready-set-go)