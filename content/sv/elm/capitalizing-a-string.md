---
title:                "Gör om en sträng till versaler"
html_title:           "Elm: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att göra första bokstav stor i en sträng innebär att ändra strängens första tecken till dess stora motsvarighet. Programmerare gör detta för att öka läsbarheten och göra datum, titlar och namn mer formella.

## Hur till:
Här är ett exempel på hur du kan göra om det första tecknet i en sträng till stort i Elm.

```Elm
capitalise : String -> String
capitalise string =
    String.toUpper (String.left 1 string) ++ String.dropLeft 1 string
```

Anropar du `capitalise "hej"` får du svaret "Hej".

## Djup Dykning:
Historiskt sett, stora bokstäver användes i början av meningar och egennamn för att indikera början på en ny tanke eller att något är unikt. Samma princip använder programmerare när de skriver kod.

Ett alternativ till `String.toUpper` + `String.left` +` String.dropLeft` i Elm kan vara att använda `String.fromChar << Char.toUpper << Char.fromCode << Char.toCode << String.left 1`.

Details i implementeringen inkluderar hur man handskas med strängar som börjar med icke-alfabetiska tecken. I de fallen returneras den ursprungliga strängen utan att ändras.

## Se Också:
- Elm: String (https://package.elm-lang.org/packages/elm/core/latest/String) för mer information om strängfunktioner i Elm.
- Elm: Char (https://package.elm-lang.org/packages/elm/core/latest/Char) för mer information om att arbeta med enskilda tecken i Elm.
- Stack Overflow: "How do you capitalize the first letter of a string in Elm?" (https://stackoverflow.com/questions/38545464/how-do-you-capitalize-the-first-letter-of-a-string-in-elm) för diskussioner om olika sätt att uppnå detta.