---
title:                "Haskell: Å finne lengden til en streng"
simple_title:         "Å finne lengden til en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Haskell er et funksjonelt programmeringsspråk som er kjent for å være elegant, effektivt og robust. En av de grunnleggende funksjonene i Haskell er evnen til å finne lengden på en streng - en handling som kan være nyttig i mange tilfeller. Enten du er en nybegynner eller en erfaren Haskell-programmerer, kan du lære hvordan du finner lengden på en streng og ta dine programmeringsferdigheter til neste nivå.

## Hvordan

Haskell tilbyr en rask og enkel måte å finne lengden på en streng gjennom bruk av funksjonen `length`. La oss se på et eksempel på hvordan dette kan gjøres i Haskell:

```Haskell
Prelude> length "Hei, verden!"

13
```

I dette eksemplet bruker vi funksjonen `length` sammen med en streng "Hei, verden!" og får som resultat lengden på 13. Denne funksjonen kan også brukes på lister, og vil returnere antall elementer i listen.

## Dykk ned

For de som er interessert i å lære mer om hvordan Haskell beregner lengden på en streng, kan vi se på implementeringen av `length`-funksjonen i Haskell. Denne funksjonen er skrevet på en rekursiv måte, som betyr at den gjentas til den når en base case hvor den returnerer lengden på strengen.

```Haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs 
```

I denne implementeringen, skiller vi mellom to tilfeller: når den tomme listen blir passert inn, returnerer den 0 siden tomme lister har en lengde på 0. I det andre tilfellet, hvor det er en ikke-tom liste, fjerner vi først det første elementet og regner én ekstra lengde for resten av listen.

## Se også

For å lære mer om Haskell og dets funksjoner, sjekk ut disse nyttige ressursene:

- [Haskell for nybegynnere](https://learnxinyminutes.com/docs/no-no/haskell-no/)
- [Offisiell Haskell-dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell-læringssamfunn](https://www.reddit.com/r/haskell/)