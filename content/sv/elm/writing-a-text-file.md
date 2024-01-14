---
title:    "Elm: Skriva en textfil"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är ett viktigt verktyg för att organisera och lagra information. Det kan vara till nytta både för personligt bruk och för professionell användning.

## Så här gör man

För att skriva en textfil i Elm behöver du först importera "Text"-modulen. Sedan kan du använda funktionen "writeFile" och ange både filnamn och innehållet som en sträng. Här är ett enkelt exempel:

```Elm
import Text exposing (writeFile)

main = 
  writeFile "hello.txt" "Hej världen!"
```

Output: En fil med namnet "hello.txt" kommer att skapas och innehålla texten "Hej världen!".

## Djupdykning

För att skriva en textfil med mer komplex innehåll kan man använda sig av funktionen "writeFileWith" som tar en modifieringsfunktion som argument. Detta gör det möjligt att använda sig av loops, if-satser och andra funktioner för att skapa en dynamisk fil. Här är ett exempel som skapar en fil med talen 1 till 10:

```Elm
import Text exposing (writeFileWith)
import List exposing (range)

main = 
  writeFileWith (\num -> String.fromInt num) "numbers.txt" (range 1 10)
```

Output: En fil med namnet "numbers.txt" kommer att skapas och innehålla siffrorna 1 till 10, varje tal på en ny rad.

## Se även

- Elm dokumentation för Text-modulen: https://package.elm-lang.org/packages/elm/core/latest/Text
- En guide för att lära sig att skriva textfiler i Elm: https://medium.com/@ajung14/writing-files-in-elm-8133b49a41d3