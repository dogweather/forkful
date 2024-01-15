---
title:                "Utvinne delstrenger"
html_title:           "Haskell: Utvinne delstrenger"
simple_title:         "Utvinne delstrenger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å hente ut substrings? 


Det kan være flere årsaker til å gjøre dette i Haskell-programmering. Kanskje du trenger å analysere en streng for å finne et bestemt mønster, eller kanskje du må manipulere tekst for å oppnå ønsket resultat. Uansett årsak, er det viktig å forstå hvordan man ekstraherer substrings i Haskell for å kunne skrive effektiv og funksjonell kode.

## Hvordan

For å ekstrahere substrings i Haskell, kan du bruke funksjonen `take` og `drop` fra `Data.List` biblioteket. Disse funksjonene tar inn en liste og et heltall som angir hvor mange elementer som skal bli tatt eller droppet. La oss se på et eksempel:

```Haskell
import Data.List

greeting = "Hei, verden!"

substring = take 3 greeting
-- Resultat: "Hei"
```

Her bruker vi `take` funksjonen til å hente de første tre bokstavene i strengen "Hei, verden!". Du kan også bruke `drop` funksjonen til å fjerne elementer fra en liste. For eksempel:

```Haskell
import Data.List

numbers = [1,2,3,4,5]

droppedNumbers = drop 2 numbers
-- Resultat: [3,4,5]
```

I tillegg til disse funksjonene, kan du også bruke `!!` operatoren for å få tilgang til en spesifikk indeks i en liste. For eksempel:

```Haskell
import Data.List

letter = "a"

alphabet = ['a'..'z']

index = alphabet !! 0
-- Resultat: 'a'
```

## Dypdykk

I tillegg til å bruke `take` og `drop` funksjonene, finnes det også flere andre måter å ekstrahere substrings på i Haskell. For eksempel kan du bruke `substring` funksjonen fra `Data.Text` biblioteket. Denne funksjonen tar inn en startindeks og en sluttindeks for å hente ut delen av en tekststreng. Her er et eksempel:

```Haskell
import Data.Text

greeting = pack "Hello, world!"

substring = substring 0 5 greeting
-- Resultat: "Hello"
```

Du kan også bruke listekomprehensjon for å ekstrahere substrings basert på et gitt kriterie. For eksempel:

```Haskell

sentence = "Jeg elsker å programmere i Haskell"

substring = [x | x <- words sentence, length x < 5]
-- Resultat: ["å", "i"]
```

Det finnes også flere andre måter å manipulere og ekstrahere substrings på i Haskell. Det viktige er å forstå de forskjellige verktøyene som er tilgjengelig og hvordan de kan hjelpe deg å oppnå ønsket resultat.

## Se også

- [Haskell dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell programmeringsspråk](https://www.haskell.org/)