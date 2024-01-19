---
title:                "Å sette en streng med store bokstaver"
html_title:           "Haskell: Å sette en streng med store bokstaver"
simple_title:         "Å sette en streng med store bokstaver"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Stor bokstavering av en streng innebærer å endre de første bokstavene i alle ord i strengen til store bokstaver. Programmerere gjør dette for å forbedre lesbarheten av tekst og følge konvensjoner innen programmeringsspråk.

## Hvordan:

```Haskell
import Data.Char(toUpper)

-- Definere funksjonen for å endre første karakter av et ord til stor bokstav
capitaliserOrd :: String -> String
capitaliserOrd [] = []
capitaliserOrd (head:tail) = toUpper head : tail

-- Bruk funksjonen på en hel setning.
capitaliserSetning :: String -> String
capitaliserSetning text = unwords . map capitaliserOrd . words $ text

-- Test
main = putStrLn $ capitaliserSetning "hello, world!"
```

Når du kjører programmet, vil utgangen være: `Hello, World!`.

## Dykk dypere 

Haskell ble introdusert i 1990, og er berømt for sin nøyaktighet. I eldre språk som C og Python, må man skrive flere linjer kode for å utføre operasjonen. Men i Haskell, kan samme operasjon gjøres med færre linjer, noe som gjør det til et mer effektivt programmeringsspråk.

Alternativer til denne funksjonen inkluderer bruk av innebygde funksjoner som `map` og `toUpper`. `map` brukes for å utføre en bestemt operasjon på hvert element i en liste eller i vårt tilfelle, på hvert ord i strengen. `toUpper` brukes for å endre små bokstaver til store bokstaver i strengen.

Implementering av større bokstaver i Haskell er enkel og tar mindre tid enn andre programmeringsspråk. Det bruker funksjonell programmerings paradigmer, og lar deg manipulere strenger effektivt.

## Se Også:

Ytterligere lesinger om string manipulasjon i Haskell på den offisielle Haskell wikien: https://wiki.haskell.org/How_to_work_on_lists

Og artikler om bruk av `map` funksjonen for å manipulere lister i Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/lists-and-tuples