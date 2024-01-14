---
title:                "Haskell: Sletting av karakterer som matcher et mønster"
simple_title:         "Sletting av karakterer som matcher et mønster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
I de fleste programmeringsspråk, inkludert Haskell, er det en vanlig utfordring å håndtere uønskede tegn i tekststrenger. Det kan være alt fra blanke mellomrom til spesielle tegn som forstyrrer korrekt utførelse av koden. I dette blogginnlegget skal vi se på hvordan vi kan slette tegn som matcher et gitt mønster i en tekststreng.

## Hvordan
For å demonstrere hvordan vi kan slette uønskede tegn i Haskell, skal vi bruke funksjonen `deleteBy` fra standardbiblioteket `Data.List`. Denne funksjonen tar inn en predikatfunksjon som bestemmer hvilke elementer som skal slettes. La oss se på et eksempel der vi ønsker å slette alle blanke mellomrom i en tekststreng:

```Haskell
import Data.List (deleteBy)

main = do
    let s = "Hei alle sammen!"
    let pattern = (== ' ')
    let cleanString = deleteBy pattern s
    putStrLn cleanString
```

Output: "Heiallesammen!"

I dette eksempelet bruker vi funksjonen `deleteBy` sammen med et predikat som tar inn et tegn og returnerer `True` hvis tegnet er et tomt mellomrom. Siden vi ønsker å slette alle blanke mellomrom, bruker vi funksjonen `==` til å sammenligne tegnet med mellomromstegnet `' '`. Deretter kan vi enkelt skrive ut resultatet ved hjelp av `putStrLn`-funksjonen.

## Dypdykk
For å forstå hvordan `deleteBy`-funksjonen fungerer, må vi se nærmere på dens signatur: `deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]`. Her ser vi at den tar inn en funksjon som sammenligner to verdier og returnerer en `Bool`, en verdi av typen `a` og en liste av verdier av typen `a`. Funktionen går gjennom listens elementer og bruker den gitte funksjonen til å bestemme hvilke elementer som skal slettes. Deretter returneres en ny liste uten disse elementene.

Et annet viktig aspekt å merke seg er at funksjonen `deleteBy` har en generell signatur som gjør at den kan brukes til å slette elementer i alle typer lister, ikke bare tekststrenger. Dette gjør den veldig fleksibel og nyttig i mange ulike situasjoner.

## Se også
- [Haskell Dokumentasjon - Data.List](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)
- [Haskell Programmering - Functors, Applicatives og Monads](https://dottyjournal.github.io/haskell-functors-applicatives-monads/)
- [Funksjonell Programmering med Haskell](https://www.itavisen.no/2019/11/15/funksjonell-programmering-med-haskell/)