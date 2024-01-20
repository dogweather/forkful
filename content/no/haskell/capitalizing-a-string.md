---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kapitalisering av en streng betyr å gjøre første bokstav i hvert ord stor. Programmerere gjør dette for å standardisere tekstvisning, for eksempel i titler eller navn.

## Hvordan gjøre det:
For å kapitalisere hver bokstav i en Haskell-streng, bruker vi en kombinasjon av innebygde funksjoner. Se på eksemplene nedenfor.

```Haskell
import Data.Char (toUpper)

-- Kapitaliserer den første bokstaven i et ord
capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : tail

-- Kapitaliserer den første bokstaven i hvert ord i en setning
capitalizeWords :: String -> String
capitalizeWords = unwords . map capitalize . words

-- Eksempel på bruk
main = do
    let sentence = "haskell programmering er gøy"
    putStrLn $ capitalizeWords sentence
```

Output:
```
"Haskell Programmering Er Gøy"
```

## Dypdykk
Kapitalisering av strenger i Haskell kan spore sine røtter tilbake til tidlige tekstbehandlingssystemer, hvor slike manipulasjoner var vanlige for å oppnå korrekt typografisk utforming. Mens Haskell standardbibliotek har basisfunksjoner som `toUpper`, finnes det flere biblioteker som `Data.Text` som tilbyr mer effektive metoder for tekstmanipulasjon.

En alternativ metode for å kapitalisere en streng er å bruke listekomprehensjon:

```Haskell
capitalizeWords' :: String -> String
capitalizeWords' s = unwords [toUpper (head w) : tail w | w <- words s, not (null w)]
```

Når det gjelder implementasjon, er det viktig å håndtere kanterilfeller, som tomme strenger eller strenger som inneholder tegn som ikke er bokstaver.

## Se Også
- Haskell sin offisielle dokumentasjon om `Data.Char`: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- `Data.Text` bibliotek for effektiv tekstmanipulasjon: https://hackage.haskell.org/package/text
- En nyttig Stack Overflow-tråd om emnet: https://stackoverflow.com/questions/1959715/how-to-make-a-char-uppercase-in-haskell