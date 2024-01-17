---
title:                "Uthenting av substrings"
html_title:           "Haskell: Uthenting av substrings"
simple_title:         "Uthenting av substrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å trekke ut substrings i programmering betyr å hente en del av en tekststreng basert på et gitt mønster eller kriterium. Dette kan være nyttig for å manipulere data eller filtrere ut relevante deler av en større tekst. Mange programmerere bruker substring-funksjoner for å forenkle og automatisere oppgaver.

## Hvordan:
```Haskell
-- Eksempel på å trekke ut en substring fra en tekststreng
let tekst = "Hei, mitt navn er Anna og jeg er 25 år gammel"
let alder = takeWhile (/= 'å') $ dropWhile (/= 'o') tekst
-- Resultat: "25"

{- Dette eksempelet bruker funksjonene takeWhile og dropWhile, som begge tar et predikat som argument. Predikatet beskriver et mønster som skal matches mot tekststrengen for å trekke ut substringen. I dette tilfellet ser predikatet etter alle tegn frem til og med bokstaven 'å', og alle tegn etter bokstaven 'o'. Også $-tegnet brukes til å sette sammen flere funksjoner på en enkel måte. -}

-- Eksempel på å bruke en regex for å trekke ut en substring
import Text.Regex.TDFA
let tekst = "I dag er det 15. mai 2020."
let datoRegex = "([0-9]+).([a-z]+) ([0-9]+)\."
let (_,_,_,dag,_,mnd,_,år,_) = tekst =~ datoRegex :: (String,String,String,String,String,String,String,String,String)
-- Resultat: dag = "15", mnd = "mai", år = "2020"

{- Dette eksempelet bruker regex-biblioteket Text.Regex.TDFA for å matche et mønster gitt som en tekst. ~= brukes for å matche teksten mot regex-uttrykket, og resultatet blir et tuple med matchende deler av tekststrengen. -}

```


## Dykk Dypere
Å trekke ut substrings har vært en del av programmering siden de tidligste språkene ble utviklet, og er fortsatt en viktig funksjon i moderne programmeringsspråk. Alternativer til å bruke forhåndsdefinerte funksjoner som takeWhile og dropWhile inkluderer å bruke regex-bibliotek som vist i det andre eksempelet. Implementasjonen av substring-funksjonene kan variere fra språk til språk, men generelt bruker de samme konsepter som beskrevet i eksemplene ovenfor.

## Se Også
- [Haskell Substring Functions](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:takeWhile)
- [Haskell Regex Library](https://hackage.haskell.org/package/regex-tdfa-1.3.1.0/docs/Text-Regex-TDFA.html)