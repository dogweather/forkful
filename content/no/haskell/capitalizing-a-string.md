---
title:                "Haskell: Stor bokstavering av en streng"
simple_title:         "Stor bokstavering av en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang ønsket å gjøre en vanlig streng mer uttrykksfull ved å ha den i stor bokstav? Vel, det er akkurat det vi skal lære om i denne bloggen - hvordan du kan kapitalisere en streng i Haskell.

## Slik gjør du det
For å kapitalisere en streng i Haskell, kan du bruke funksjonen `toUpper` fra `Data.Char`. Denne funksjonen tar inn en char og returnerer den samme char i stor bokstav. La oss se på et eksempel:

```Haskell
import Data.Char (toUpper)

streng = "velkommen"
kapitalisert = map toUpper streng

print kapitalisert
```

Dette vil gi output av `"VELKOMMEN"`, siden `map` funksjonen gjør om hver char i strengen `streng` til stor bokstav ved hjelp av funksjonen `toUpper`.

## Dypdykk
For å forstå mer om hvordan dette fungerer, la oss se nærmere på `map` funksjonen som vi bruker i eksempelet over. `map` tar inn en funksjon og en liste, og returnerer en ny liste der funksjonen er blitt brukt på hvert element i den originale listen. I vårt eksempel, bruker vi `map` til å bruke `toUpper` funksjonen på hver char i strengen `streng`.

Vi bruker også `import Data.Char (toUpper)` for å importere funksjonen `toUpper` fra `Data.Char` modulen. Dette lar oss bruke funksjonen i koden vår. Husk å alltid importere de nødvendige modulene for å kunne bruke funksjonene du trenger.

## Se også
- [Offisiell Haskell nettsted](https://www.haskell.org/)
- [Haskell kurs på Khan Academy](https://www.khanacademy.org/computing/computer-programming)
- [Offisiell Haskell dokumentasjon](https://www.haskell.org/documentation/)