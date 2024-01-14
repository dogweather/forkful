---
title:                "Haskell: Att hitta längden på en sträng"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en vanlig uppgift inom programmering och kan vara användbar i många olika situationer. Genom att lära sig hur man gör detta i Haskell kan du förbättra din kodning och effektivisera ditt arbete.

## Hur man gör
För att börja med, låt oss definiera en sträng i Haskell. Detta görs enkelt genom att omge text med citattecken, till exempel "Hej!". För att hitta längden på en sträng använder vi funktionen `length`. Låt oss titta på ett exempel:

```Haskell
strang = "Hej!"
length strang
```

Detta kommer att ge oss en utgång på 4 eftersom det finns 4 tecken i strängen "Hej!".

Vi kan också använda `length` på mer komplexa strängar, till exempel en som innehåller speciella tecken och mellanslag:

```Haskell
strang = "Hej! Detta är en komplex sträng..."
length strang
```

Resultatet blir 32 eftersom det finns 32 tecken i strängen inklusive mellanslaget.

## Djupdykning
I Haskell är `length` en fördefinierad funktion som tillhör den inbyggda typen `List`. Detta innebär att det finns en rad olika sätt att anpassa och ändra funktionen beroende på dina behov. Till exempel kan du skriva en egen version av `length` som använder rekursion:

```Haskell
customLength [] = 0
customLength (x:xs) = 1 + customLength xs
```

Denna funktion använder mönstermatchning för att ta hänsyn till en tom lista, vilket ger ett basfall, och sedan räknar antalet element i listan genom att använda rekursion tills den är tom.

Det finns också andra funktioner som kan användas tillsammans med `length` för att utföra mer avancerade operationer, som `take` och `drop`. Dessa kan användas för att extrahera en viss mängd tecken från en sträng och sedan använda `length` för att hitta längden på den nya strängen.

## Se även
- [Haskell Wiki: Lists](https://wiki.haskell.org/Lists)
- [Haskell for all: The List Type](http://www.haskellforall.com/2016/03/the-list-type-class.html)
- [Real World Haskell: Lists in Data.List](http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html#workingWithLists_lang_exercises)