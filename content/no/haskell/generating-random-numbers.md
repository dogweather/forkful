---
title:                "Haskell: Generering av tilfeldige tall"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmer krever tilfeldige tall for å skape variasjon og unngå forutsigbarhet. Enten det er for å lage spill, simulere data eller generere tilfeldige skyer i et landskapsbilde, så er tilfeldige tall en viktig funksjon i mange programmeringsprosjekter. I denne bloggposten skal vi se på hvordan vi kan generere tilfeldige tall ved hjelp av Haskell.

## Hvordan gjøre det?

Før vi begynner å kode, må vi importere modulen som lar oss generere tilfeldige tall:

```Haskell
import System.Random
```

For å generere et tilfeldig tall mellom 0 og 10, kan vi bruke funksjonen `randomRIO` fra `System.Random`-modulen. Denne funksjonen tar inn en tuple som spesifiserer intervallet for det tilfeldige tallet. Vi kan også angi datatypen vi ønsker å få tilbake, for eksempel `Int` eller `Float`.

```Haskell
tilfeldigTall <- randomRIO (0, 10) :: IO Int
print tilfeldigTall
```

Hver gang vi kjører denne koden, vil vi få et nytt tilfeldig tall mellom 0 og 10 som output. Vi kan også generere flere tilfeldige tall ved hjelp av en løkke:

```Haskell
tilfeldigeTall <- sequence $ replicate 5 (randomRIO (0, 10) :: IO Int)
print tilfeldigeTall
```

Dette vil generere 5 tilfeldige tall mellom 0 og 10 og lagre dem i en liste.

## Dypdykk

I Haskell er tilfeldige tall ikke generert direkte, men heller basert på en tilfeldig nummergenerator (rng). Denne generatoren tar inn et seed-nummer, og bruker dette til å generere en sekvens av tall som kan anses som tilfeldige ved bruk av kompliserte algoritmer. Dette betyr at dersom vi gir samme seed-nummer, vil vi alltid få den samme sekvensen av tilfeldige tall.

Vi kan også angi en egen rng-funksjon ved å bruke `mkStdGen`. Dette gir oss mer kontroll over den tilfeldige nummergeneratoren, og kan være nyttig i visse situasjoner.

## Se også

- [Haskell Wiki - Random Numbers](https://wiki.haskell.org/Random_numbers)
- [Haskell Documentation - System.Random](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Learn You a Haskell for Great Good! - Randomness](http://learnyouahaskell.com/input-and-output#randomness)