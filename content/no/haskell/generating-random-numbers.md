---
title:    "Haskell: Generering av tilfeldige tall"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er viktig i mange programmeringsspråk, inkludert Haskell. Randomisering er en nøkkelfunksjon for å skape variasjon og tilfeldighet i koden din. Dette kan være nyttig for simuleringer, spill og sikkerhetsfunksjoner. Ikke bare det, det kan også gjøre programmeringen mer interessant og utfordrende.

## Hvordan

Å generere tilfeldige tall i Haskell er enkelt, du trenger bare å importere "System.Random" biblioteket og bruke funksjonen "randomR". Her er et eksempel på å generere et tilfeldig tall mellom 1 og 10:

```Haskell
import System.Random

main = do
  let num = randomR (1,10) :: IO Int
  randomNum <- num
  putStrLn ("Det tilfeldige tallet er: " ++ show randomNum)
```

Output:
```Haskell
Det tilfeldige tallet er: 5
```

## Deep Dive

For å forstå hvordan tilfeldige tall blir generert i Haskell, må vi dykke litt dypere inn i "System.Random" biblioteket. Biblioteket inneholder en rekke funksjoner for å generere tilfeldige tall, inkludert "randomR" som brukes i eksempelet ovenfor.

For å generere tilfeldige tall, bruker Haskell en teknikk kalt pseudorandom number generation (PRNG). Dette betyr at tallene ikke er helt tilfeldige, men genereres av en algoritme basert på en startverdi kalt "seed". Hvis du gir samme seed til en PRNG, vil den alltid generere den samme sekvensen av tall.

Det er viktig å velge en god seed for å få en "god" tilfeldig tallgenerering. Haskell har også en "mkStdGen" funksjon som lar deg velge en bestemt seed for å generere tilfeldige tall.

## Se også

- [Haskell.org sin dokumentasjon om tilfeldige tall](https://wiki.haskell.org/Random_number_generation)
- [StackOverflow post om tilfeldig tallgenerering i Haskell](https://stackoverflow.com/questions/9066951/making-random-numbers-in-haskell)