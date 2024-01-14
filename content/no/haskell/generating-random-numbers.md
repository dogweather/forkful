---
title:    "Haskell: Generering av tilfeldige tall"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Random nummergenerering er en viktig del av programmering. Det tillater oss å skape tilfeldige verdier som kan brukes til å legge til variasjon og spenning i våre programmer.

## Hvordan

For å generere tilfeldige tall i Haskell, bruker vi funksjonen `random` fra `System.Random` biblioteket. Her er et eksempel på hvordan vi kan generere et tilfeldig tall mellom 1 og 10:

```Haskell
import System.Random (randomR, newStdGen)

-- Generer et tilfeldig tall mellom 1 og 10
randomNum :: IO Int
randomNum = do
  gen <- newStdGen
  let (num, _) = randomR (1, 10) gen
  return num
```
Output:
```
7
```

Vi først importerer `randomR` funksjonen som tillater oss å generere et tilfeldig tall innenfor et gitt område, og `newStdGen` funksjonen som genererer en tilfeldig generator. Deretter bruker vi `let` uttrykksblokken for å tilordne et tilfeldig tall til variabelen `num` og ignorerer den andre verdien som returneres fra `randomR`. Til slutt returnerer vi `num` fra funksjonen.

## Dypdykk

`System.Random` biblioteket tilbyr også andre nyttige funksjoner for å generere tilfeldige verdier, som for eksempel `randomIO` som returnerer et tilfeldig tall i et hvilket som helst område. Dette biblioteket bruker generatoren som er definert av operativsystemet, noe som sikrer at de genererte tallene virkelig er tilfeldige.

Det er viktig å merke seg at tilfeldige tall ikke er helt tilfeldige, men det er en algoritme som brukes til å generere dem. Derfor kan tilfeldige tallgenerering være sårbar for visse typer angrep. Det er derfor viktig å bruke tilfeldige tall fra en pålitelig kilde, som `System.Random` biblioteket, i stedet for å skrive vår egen tilfeldige tallgenerator.

## Se også

- [Haskell random library documentation](https://hackage.haskell.org/package/random)
- [Using Randomness in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Randomness)
- [Randomness in Purely Functional Programs](http://www.cse.chalmers.se/~rjmh/Papers/generating.ps)