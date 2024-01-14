---
title:                "Elm: Huvudstavigering av en sträng"
simple_title:         "Huvudstavigering av en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna manipulera strängar är en grundläggande färdighet i programmering. Genom att lära dig hur man gör en sträng stor eller liten kan du anpassa ditt program efter användarens behov och skapa en bättre användarupplevelse.

## Hur man gör

Att göra en sträng stor eller liten kan göras med hjälp av inbyggda funktioner i Elm.

```Elm
-- För att göra en sträng stor:
String.toUpper "hej"  -- ger "HEJ"

-- För att göra en sträng liten:
String.toLower "HEJ"  -- ger "hej"
```

Dessa funktioner är användbara för att till exempel anpassa text på en knapp eller i en textinput. Genom att kombinera dem med andra funktioner kan du skapa mer komplexa strängmanipulationer.

## Djupdykning

För att förstå hur dessa funktioner fungerar under ytan kan det vara bra att titta på hur strängar är representerade i Elm. Strängar är egentligen bara listor av tecken, vilket gör att vi kan använda List-funktioner på dem.

Till exempel används `List.map` för att applicera en funktion på varje element i en lista. I fallet med att göra en sträng stor eller liten applicerar funktionen `Char.toUpper` eller `Char.toLower` på varje tecken i strängen.

## Se även

- [Officiell Elm dokumentation om strängar](https://elm-lang.org/docs/strings)
- [Elm guide om listor](https://guide.elm-lang.org/lists/)
- [Elm community-forum](https://discourse.elm-lang.org/)