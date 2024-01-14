---
title:                "Haskell: Ekstrahering av understrenger"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med tekstbehandling er en viktig del av å være en Haskell-programmerer. Et vanlig problem som kan oppstå er å trenge å trekke ut visse deler av en tekststreng, eller substrings som det heter på engelsk. Derfor er det viktig å være kjent med hvordan man kan gjøre dette i Haskell for å effektivt behandle tekst.

## Hvordan

Effektivt å trekke ut substrings i Haskell kan gjøres ved å bruke funksjonen `take` og `drop`. Disse funksjonene tar inn et tall og en tekststreng som argumenter, og returnerer henholdsvis de første eller siste tegnene av strengen basert på tallet som er gitt. Her er et eksempel på hvordan funksjonene kan brukes:

```Haskell
-- Definerer en tekststreng
tekst = "Dette er en tekststreng."

-- Trekker ut de første 10 tegnene
substring1 = take 10 tekst
-- Output: "Dette er e"

-- Trekker ut de siste 10 tegnene
substring2 = drop (length tekst - 10) tekst
-- Output: "tekststreng."
```

Ved å bruke `take` og `drop` kan man altså enkelt trekke ut deler av en tekststreng basert på plasseringen i strengen.

## Dypdykk

I tillegg til `take` og `drop`, finnes det også en rekke andre nyttige funksjoner for å håndtere substrings i Haskell. Blant annet kan `takeWhile` og `dropWhile` brukes for å trekke ut tegn basert på et gitt kriterium, for eksempel hvis de skal være tall eller bokstaver.

Det finnes også funksjoner som `words` og `lines` som kan brukes til å dele en tekststreng opp i ord eller linjer. Dette kan være nyttig hvis man for eksempel ønsker å telle antall ord eller linjer i en tekst.

## Se også

- [Haskell Strings - Extracting substrings](https://www.tutorialspoint.com/haskell_strings/haskell_strings_extracting_substrings.htm)
- [Haskell Strings - Advanced operations](https://www.tutorialspoint.com/haskell_strings/haskell_strings_advanced_operations.htm)
- [Official Haskell documentation - String functions](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html)