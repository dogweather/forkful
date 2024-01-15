---
title:                "Extrahera substrängar"
html_title:           "Elm: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Är du trött på att klippa och klistra text för hand? Vill du enkelt kunna extrahera delar av en sträng? Då är det här guiden för dig! Vi kommer att gå igenom hur du använder Elm för att enkelt extrahera substrängar och spara tid och frustration.

## Hur man gör det

För att extrahera en substräng i Elm behöver vi komma åt två funktioner: `String.slice` och `String.length`. Låt oss först titta på `String.slice`, som låter oss extrahera en del av en sträng baserat på dess start- och slutindex. Här är ett exempel på hur vi kan använda den:

```Elm
substring = String.slice 6 11 "Hej där, världen!"
```

I det här exemplet kommer `substring` att vara lika med "världen". Vi använder indexen 6 och 11 eftersom de markerar starten och slutet av substrängen vi vill extrahera. Nu kanske du undrar, hur vet jag vilka index jag ska använda? Det är där `String.length` kommer in. Genom att använda denna funktion kan vi ta reda på längden på strängen och därmed bestämma vilka index vi behöver. Här är ett komplett exempel:

```Elm
sträng = "Jag gillar att lära mig Elm"
längd = String.length sträng

substring = String.slice 12 längd sträng
```

I det här exemplet kommer `substring` att vara lika med "lära mig Elm". Vi har använt `String.length` för att ta reda på att strängen är 26 tecken lång och sedan använt den som slutindex för `String.slice`. Genom att använda dessa funktioner kan du enkelt extrahera valfri del av en sträng utan att behöva räkna ut index manuellt.

## Djupdykning

Nu när du vet hur du extraherar substrängar är det viktigt att förstå hur indexering fungerar i Elm. Indexering börjar alltid på 0, vilket innebär att det första tecknet i en sträng har index 0 och inte 1. Detta är viktigt att komma ihåg när du använder `String.slice` eftersom det kan vara förvirrande att veta vilket index som ska användas. En annan viktig punkt att notera är att indexet för slutet av substrängen borde vara ett tal som är mindre än indexet för starten av substrängen. Om vi till exempel ville extrahera "världen" från "Hej där, världen!" skulle vi behöva ange 6 som startindex och 12 som slutindex, inte 11.

## Se även

- Elm Dokumentation för `String.slice`: https://package.elm-lang.org/packages/elm/core/latest/String#slice
- Elm Dokumentation för `String.length`: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Elm forumtråd om substrängar: https://discourse.elm-lang.org/t/substring/1602