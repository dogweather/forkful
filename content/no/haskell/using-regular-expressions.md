---
title:                "Haskell: Å bruke regulære uttrykk"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke regulære uttrykk kan virke som en skremmende oppgave for mange programmerere, men det kan hjelpe deg med å effektivisere og forenkle kode. Regulære uttrykk lar deg søke, erstatte og manipulere tekst på en effektiv måte.

## Hvordan

For å bruke regulære uttrykk i Haskell, må du først importere "Text.Regex.TDFA" modulen. Deretter kan du bruke funksjonen "makeRegex" for å lage et regulært uttrykk. For eksempel, hvis vi vil finne alle tall i en tekststreng, kan vi bruke følgende kode:

```Haskell
import Text.Regex.TDFA

tekst = "Jeg er 27 år gammel"

regex = makeRegex "[0-9]+" :: Regex

match = matchAll regex tekst :: [[String]]
```

I dette eksempelet bruker vi "makeRegex" for å definere et mønster som skal søkes etter, i dette tilfellet alle tall. Deretter bruker vi funksjonen "matchAll" for å finne alle forekomster av dette mønsteret i teksten vår. Resultatet vil bli en liste av strenger som inneholder de matchende tallene, i dette tilfellet ["27"].

## Dypt dykk

Regulære uttrykk følger et sett med syntaksregler for å beskrive strenger som skal matches. Dette inkluderer spesielle karakterer som kan brukes til å representere ulike typer bokstaver, tall eller symboler. For å lære mer om disse syntaksreglene og hvordan de kan brukes til å lage mer avanserte regulære uttrykk, kan du se på dokumentasjonen for "Text.Regex.TDFA" modulen.

## Se også

- [Tekst.Regex.TDFA dokumentasjon](https://hackage.haskell.org/package/regex-tdfa-1.3.1.0/docs/Text-Regex-TDFA.html)
- [Haskell Regex Guide](https://wiki.haskell.org/Regular_expressions)
- [Regex Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/haskell)