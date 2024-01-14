---
title:                "Elm: Å finne lengden til en streng"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en viktig del av programmering, enten du jobber med webutvikling, dataanalyse eller spillutvikling. Det gir deg muligheten til å håndtere og manipulere tekster på en effektiv måte.

## Hvordan

For å finne lengden på en streng i Elm, kan du bruke funksjonen `String.length`. Denne funksjonen tar inn en streng som argument og returnerer lengden på strengen i form av et heltall. La oss se på et eksempel:

```Elm
import String exposing (length)

string = "Hei, verden!"
lengde = length string

-- Output: lengde = 12
```

I dette eksempelet importerer vi `String.length`-funksjonen og oppretter en streng med verdien "Hei, verden!". Deretter finner vi lengden på denne strengen ved å bruke `length`-funksjonen og lagrer resultatet i en variabel. Når vi skriver ut verdien av variabelen `lengde`, får vi svaret som er 12, siden strengen består av 12 tegn.

## Dypdykk

Hvis du vil gå dypere inn i hvordan `String.length`-funksjonen fungerer, kan du undersøke kildekoden på Elm sin offisielle nettside. En annen viktig ting å huske på når du jobber med strenger i Elm, er at de er uforanderlige og ikke kan endres. Dette betyr at når du bruker `String.length`-funksjonen, vil den ikke påvirke den opprinnelige strengen, men bare gi deg lengden på den.

## Se Også

- [Offisiell Elm nettside](https://elm-lang.org)
- [Elm dokumentasjon for strenger](https://package.elm-lang.org/packages/elm/core/latest/String)