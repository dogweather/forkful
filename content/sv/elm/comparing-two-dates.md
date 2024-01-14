---
title:    "Elm: Jämföra två datum"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

I denna bloggpost ska vi prata om hur man jämför två datum i Elm-programmering och varför det är en användbar färdighet att ha. Att kunna jämföra datum är viktigt i många olika typer av applikationer, till exempel när man vill visa information baserat på ett specifikt datum eller sortera data baserat på datum.

## Hur man jämför två datum i Elm

För att jämföra två datum i Elm behöver vi använda funktionen `Date.compare`. Denna funktion tar in två datum som argument och jämför dem baserat på deras storlek. Om det första datumet är mindre än det andra returneras värdet `LT` (som står för "Less Than"), om det första datumet är större returneras `GT` (som står för "Greater Than") och om datumen är lika returneras `EQ` (som står för "Equal").

Låt oss titta på ett exempel på hur man gör detta i Elm:

```Elm
Date.compare (Date.fromCalendarDate 2021 11 12) (Date.fromCalendarDate 2021 11 11)
```

I detta exempel jämför vi två datum: 12 november 2021 och 11 november 2021. Eftersom 12 november är större än 11 november kommer funktionen `Date.compare` att returnera `GT`.

Man kan också använda funktionen `Date.compare` för att sortera en lista av datum, vilket gör det enklare att visa information i rätt ordning. Låt oss se ett exempel på detta:

```Elm
['2021-11-10', '2021-11-12', '2021-11-11']
    |> List.map Date.fromIsoString
    |> List.sort Date.compare
```

I detta exempel använder vi `List.sort` för att sortera en lista av datumsträngar. Först konverterar vi dessa strängar till datum med hjälp av funktionen `Date.fromIsoString`. Sedan använder vi funktionen `Date.compare` för att jämföra datumen och sortera dem i rätt ordning. Slutresultatet blir en lista med datumen i stigande ordning: ['2021-11-10', '2021-11-11', '2021-11-12'].

## Djupdykning

För de som är intresserade av att lära sig mer om hur man jämför datum i Elm, så finns det vissa saker att tänka på. För det första, när man använder funktionen `Date.compare`, så måste datumen vara av samma typ. Detta innebär att både datumen måste vara av typen `Date` eller båda måste vara av typen `Time`. Om man försöker jämföra en `Date` med en `Time` kommer funktionen att ge ett felmeddelande.

En annan viktig sak att tänka på är att `Date.compare` inte tar hänsyn till tidszonen. Om man vill jämföra datumen inklusive tidszonen, så kan man använda funktionen `Date.toPosix` innan man jämför dem.

## Se även

- Elm dokumentation för `Date.compare`: https://package.elm-lang.org/packages/elm/time/latest/Date#compare
- Elm dokumentation för `List.sort`: https://package.elm-lang.org/packages/elm/core/latest/List#sort
- Elm dokumentation för `Date.toPosix`: https://package.elm-lang.org/packages/elm/time/latest/Date#toPosix