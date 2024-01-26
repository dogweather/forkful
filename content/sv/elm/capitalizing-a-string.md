---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att sätta stor bokstav ("capitalize") betyder att ändra första bokstaven i en sträng till versal. Programmerare gör detta för att formatera texter, som i titlar eller för att följa språkliga konventioner.

## How to:
Elm har inget inbyggt bibliotek för att göra strängar till versaler. Men vi kan bygga en funktion själva. Här är ett enkelt exempel:

```Elm
import String exposing (toUpper, left, dropLeft)

capitalize : String -> String
capitalize str =
  let
    firstChar =
      str
        |> left 1
        |> toUpper
    restOfString =
      dropLeft 1 str
  in
    firstChar ++ restOfString

-- Användning
capitalize "hej värld"
-- Output: "Hej värld"
```

## Deep Dive
Elm är byggt för funktionell programmering och sidoeffektsfrihet, därför finns det inga metoder som direkt modifierar strängar; du behöver skapa nya strängar istället.

Historiskt sett har andra språk som JavaScript haft inbyggda metoder som `.toUpperCase()`, men i Elm föredrar vi rena funktioner för mer förutsägbar kod. Ett alternativ är att använda ett paket som `elm-string-extra`, men att känna till grunderna är alltid användbart.

Implementationsdetaljer: `toUpper` konverterar alla bokstäver till versaler, vilket är överflödigt om vi bara vill ha första bokstaven i versal. Därför använder vi `left` för att få första tecknet, gör det till versal, och `dropLeft` för att slänga första tecknet och sammanfoga resten utan ändring.

## See Also
- Elm String documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm package for extra string functions: https://package.elm-lang.org/packages/pzp1997/elm-string-extra/latest/
