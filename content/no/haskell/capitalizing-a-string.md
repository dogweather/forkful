---
title:    "Haskell: Stor bokstaver i en streng"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å kapitalisere en streng? Vel, det kan være mange grunner til det. Kanskje du ønsker å gjøre navn i en liste mer leselige, eller du vil formatere en overskrift på en nettside. Uansett hva grunnen måtte være, er det en nyttig operasjon å kunne gjøre i Haskell.

## Hvordan gjøre det

Å kapitalisere en streng i Haskell er enkelt. Alt du trenger å gjøre er å bruke funksjonen "toUpper". La oss ta en titt på et eksempel:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str

main = do
    let str = "dette er en test"
    putStrLn $ capitalize str
```

Kjører denne koden vil gi følgende output:

```
DETTE ER EN TEST
```

Som du kan se, har "toUpper" transformert hele strengen til store bokstaver. La oss nå se på et annet eksempel som viser hvordan vi kan kapitalisere bare første bokstav i en streng:

```Haskell
import Data.Char (toUpper)

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs

main = do
    let str = "dette er en test"
    putStrLn $ capitalizeFirst str
```

Output fra denne koden vil være:

```
Dette er en test
```

Her bruker vi funksjonen "toUpper" til å transformere den første bokstaven, og deretter legger den til resten av strengen ved hjelp av "map". Dette er bare to enkle eksempler, men du kan eksperimentere med flere måter å kapitalisere strenger på.

## Dypdykk

For de som er interessert i å få en dypere forståelse av hvordan kapitalisering av strenger fungerer i Haskell, er det noen ting verdt å merke seg. Først og fremst bruker vi "Data.Char" modulen for å få tilgang til "toUpper" funksjonen. Denne modulen inneholder også andre funksjoner for å manipulere bokstaver i en streng, som for eksempel "toLower" og "isAlpha" (sjekker om en bokstav er en bokstav eller et annet tegn).

Det er også viktig å merke seg at i Haskell er strenger faktisk lister av tegn. Dette betyr at vi kan bruke funksjoner som "map" og "filter" for å manipulere dem. Det er også mulig å bruke rekursjon og mønstergjenkjenning for å oppnå samme resultat som vi gjorde med våre to eksempler ovenfor.

## Se også

- [Haskell.org](https://www.haskell.org/)
- [The Haskell Wiki](https://wiki.haskell.org/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)