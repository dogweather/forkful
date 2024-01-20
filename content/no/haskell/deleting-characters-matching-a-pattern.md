---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---

# Sletting av tegn som samsvarer med et mønster i Haskell
*Av Contract Author*

---

## Hva & Hvorfor?

Sletting av tegn som samsvarer med et mønster er en prosess for å fjerne sekvenser av tegn basert på et spesifikt mønster eller regler. Programmerere bruker dette til å håndtere og behandle data mer effektivt.

---

## Hvordan:

Her er et eksempel på hvordan vi kan fjerne tegn som samsvarer med et mønster i Haskell.

```Haskell
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
```

Dette eksemplet viser hvordan vi kan bruke `dropWhileEnd` funksjonen til å fjerne mellomrom på begge ender av en streng.

```Haskell
main = print (trim "   Hei, Universe   ")  -- Output: "Hei, Universe"
```

---

## Dypdykk

Historisk sett har sletting av tegn som samsvarer med et mønster lenge vært en viktig del av tekstbehandling i programmering. Selv om det finnes mange metoder for å oppnå dette, demonstrerer Haskell's funksjonelle programmeringsevner hvor kraftig og enkelt det kan være. Alternativer inkluderer bruk av regulære uttrykk og skrivemåter som er mer detaljerte, men det vil avhenge av den spesifikke applikasjonen og avanserte krav.

Ved implementering i Haskell, er sletting av tegn typisk oppnådd ved hjelp av innebygde funksjoner som `dropWhile` og `dropWhileEnd`. Disse funksjonene opererer på lister, og i tilfelle av strenger, behandler Haskell strenger som lister av tegn.

---

## Se også

* [Haskell offisielle dokumentasjon om List processing functions](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:dropWhile)

* [Lær deg Haskell, kapittel om lister](http://learnyouahaskell.com/starting-out#ready-set-go) 

* [Stack Overflow Haskell spørsmål om character pattern deletion](https://stackoverflow.com/questions/9263651/how-do-i-delete-all-characters-in-string-that-match-a-pattern)