---
title:    "Elm: Att hitta längden på en sträng"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Varför
Att hitta längden av en sträng är en viktig grundläggande färdighet i alla programmeringsspråk, och Elm är inget undantag. Det är en viktig del av att hantera och manipulera textdata, och är en färdighet som kommer att vara användbar i många olika projekt.

## Hur man gör det
För att hitta längden av en sträng i Elm, behöver vi bara använda inbyggda funktionen `String.length`. Låt oss titta på ett exempel:

```Elm
let namn = "Johan"
String.length namn
```

Detta kommer att ge oss en output på `5`, eftersom det är antalet tecken i strängen "Johan". Vi kan också använda den här funktionen direkt på en hårdkodad sträng:

```Elm
String.length "Hej världen!"
```

Detta kommer att ge oss en output på `12`, eftersom det är längden på denna sträng.

## Djupdykning
Det är också möjligt att hitta längden av en sträng med hjälp av en for-loop. Detta är dock inte den rekommenderade metoden eftersom det är onödigt komplicerat och tar längre tid än att bara använda `String.length` funktionen. En annan viktig sak att notera är att längden av en sträng mäts i antal tecken, inte antal ord. Så även om en sträng innehåller flera ord, kommer `String.length` funktionen bara att räkna antal tecken.

## Se även
- [Elm dokumentation för String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [W3Schools för String.length i Elm](https://www.w3schools.com/ELM/elm_functions_strings.asp)