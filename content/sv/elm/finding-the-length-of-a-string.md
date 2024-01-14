---
title:                "Elm: Att hitta längden av en sträng"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden av en sträng kan verka som en enkel uppgift, men det finns faktiskt flera olika sätt att göra det på. Att veta hur man hittar längden på en sträng kan vara användbart i många olika programmeringssituationer.

## Hur Du Gör

Att hitta längden på en sträng i Elm är ganska enkelt med hjälp av den inbyggda funktionen `String.length`. Denna funktion tar en sträng som argument och returnerar längden av strängen som ett heltal. Här är ett exempel på kod som visar hur man använder denna funktion:

```Elm
stringLength = String.length "Hej, världen!"
```

Detta resulterar i en längd av 13 eftersom det finns 13 tecken i strängen "Hej, världen!". Om man istället vill använda `String.length` för att hitta längden på en variabel sträng, kan man göra så här:

```Elm
myString = "Detta är en variabel sträng."
stringLength = String.length myString
```

Det är också möjligt att använda `String.length` för att hitta längden på en tom sträng. I detta fall kommer funktionen att returnera värdet 0.

## Djupdykning

När man använder `String.length` i Elm bör man vara medveten om att funktionen räknar antalet tecken och inte antalet ord. Detta innebär att tecknen "ä", "å", och "ö" kommer att räknas som ett enda tecken i strängen. Detta är viktigt att tänka på när man ska formatera utmatningen eller arbeta med strängar som innehåller specialtecken.

Det är också värt att notera att `String.length` inte bara fungerar för vanliga strängar utan också för teckenlistor, vilket gör den användbar för olika typer av data.

## Se Också

- Elm dokumentation för `String.length`: https://package.elm-lang.org/packages/elm/core/latest/String#length
- En guide för att arbeta med strängar i Elm: https://guide.elm-lang.org/strings/
- Exempel på hur man använder `String.length` i praktiken: https://www.techopedia.com/definition/17257/string-length