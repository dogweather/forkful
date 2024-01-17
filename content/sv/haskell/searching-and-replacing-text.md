---
title:                "Söka och ersätta text"
html_title:           "Haskell: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text är en vanlig uppgift för programmerare. Genom att hitta och ersätta specifika ord eller fraser i en text kan vi effektivt ändra eller uppdatera innehållet. Detta är särskilt användbart när vi arbetar med stora mängder text, som i programkod eller dokumentation.

## Så här:

```Haskell
-- Ett enkelt exempel på hur vi kan söka och ersätta text i Haskell:

-- Skapa en funktion som tar in en sträng, sökordet och ersättningstexten som argument
replaceText :: String -> String -> String -> String
replaceText input search replace =
  if null input -- Kolla om input-strängen är tom
     then [] -- Om den är tom, returnera en tom sträng
     else if take (length search) input == search -- Om sökordet matchar början av input-strängen
             then replace ++ replaceText (drop (length search) input) search replace -- Lägg till ersättningstexten och sök igen i resten av strängen
             else [head input] ++ replaceText (tail input) search replace -- Annars lägg till första tecknet i strängen och sök igen i resten

-- Testa funktionen genom att söka och ersätta "vän" med "kompis" i en text
main = putStrLn $ replaceText "Hej min vän, hur mår du?" "vän" "kompis"

-- Resultatet blir: "Hej min kompis, hur mår du?"
```

## Djupdykning:

Sökning och ersättning av text är en viktig del av programmering och har funnits sedan de första programmeringsspråken utvecklades. Innan det fanns inbyggda funktioner för detta var det vanligt att programmerare behövde skriva sina egna algoritmer för att uppnå samma resultat.

Det finns också alternativ till sökning och ersättning av text, som reguljära uttryck, som är mer kraftfulla och mer flexibla i vissa situationer. Dessa är dock mer komplexa och kräver mer avancerad kunskap för att användas effektivt.

I Haskell finns det inbyggda funktioner som gör det enkelt att söka och ersätta text. Bland annat har vi funktionerna "take", "drop", "length" och "++" som används i vårt exempel ovan. Genom att kombinera dessa funktioner kan vi skapa en effektiv och enkel funktion för sökning och ersättning av text.

## Se även:

- [Haskells String-modul](https://hackage.haskell.org/package/base/docs/Data-String.html)
- [Reguljära uttryck i Haskell](https://wiki.haskell.org/Regular_expressions)