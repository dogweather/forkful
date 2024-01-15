---
title:                "Stor forbokstav i en streng"
html_title:           "Haskell: Stor forbokstav i en streng"
simple_title:         "Stor forbokstav i en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Å kapitalisere en streng er en vanlig operasjon i programmering, spesielt når du jobber med tekstbehandling. Dette betyr å gjøre den første bokstaven i hver ord til en stor bokstav, og det kan være nyttig for presentasjon eller sortering av data.

# Hvordan gjøre det

Den enkleste måten å kapitalisere en streng i Haskell er å bruke funksjonen "toUpper" fra "Data.Char" biblioteket. Her er et eksempel på hvordan du kan bruke det:

```Haskell
import Data.Char (toUpper)

capitalizeString :: String -> String
capitalizeString str = map toUpper str

-- sample input and output
capitalizeString "hello world" -- "Hello World"
capitalizeString "haskell is fun" -- "Haskell is fun"
```

Først importerer vi "toUpper" funksjonen fra "Data.Char" biblioteket. Deretter definerer vi vår egen funksjon "capitalizeString" som tar inn en streng som parameter. Inne i funksjonen bruker vi "map" funksjonen til å gå gjennom hver bokstav i strengen og bruke "toUpper" på den. Dette gjør at bokstavene blir konvertert til store bokstaver. Til slutt kan vi teste vår funksjon med sample input og se den kapitaliserte strengen som output.

# Dypdykk

Haskell har også flere andre metoder for å kapitalisere en streng. For eksempel kan du bruke funksjonen "toTitle" fra "Data.Text" biblioteket for å kapitalisere hver føste bokstav i hvert ord i en tekststreng. Det er også mulig å bruker "Data.Word" biblioteket for å konvertere en streng til en liste av ord og deretter kapitalisere hver første bokstav i hvert ord.

Det er viktig å merke seg at disse operasjonene ikke endrer den opprinnelige strengen, men returnerer en ny streng med de kapitaliserte endringene. Derfor er det viktig å tildele den kapitaliserte strengen til en variabel eller bruke den i en funksjon hvis du ønsker å beholde endringene.

# Se også

- [Haskell dokumentasjon](https://www.haskell.org/documentation/)
- [Tutorial på kapitalisering av strenger](https://wiki.haskell.org/Tutorials/Programming_Haskell/String_IO)
- [Data.Char biblioteket](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Data.Text biblioteket](https://hackage.haskell.org/package/text-1.2.3.2/docs/Data-Text.html)
- [Data.Word biblioteket](https://hackage.haskell.org/package/base/docs/Data-Word.html)