---
title:                "Haskell: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig del av mange programmeringsoppgaver, spesielt når det kommer til å teste og simulere ulike situasjoner. Ved å skape tilfeldige tall kan vi imitere virkeligheten og bidra til å løse problemer som krever en viss grad av usikkerhet.

## Slik gjør du det

```Haskell
-- Importerer biblioteket for å generere tilfeldige tall
import System.Random

-- Funksjon for å generere et tilfeldig tall mellom 0 og 10
randomNumber :: IO Int
randomNumber = randomRIO (0, 10)

-- Eksempel på hvordan funksjonen kan brukes
main :: IO ()
main = do
    -- Lagrer det tilfeldige tallet i en variabel
    tall <- randomNumber
    -- Skriver ut tallet
    putStrLn $ "Det tilfeldige tallet er: " ++ show tall
```

Dette vil gi følgende output:

```
Det tilfeldige tallet er: 8
```

## Dykk dypere

I Haskell bruker vi funksjonen `randomRIO` fra `System.Random` biblioteket for å generere tilfeldige tall. Denne funksjonen tar imot et tuple med to verdier, og genererer et tilfeldig tall mellom disse to verdiene. For å bruke en rekke med tilfeldige tall, kan vi også bruke `randomRs` funksjonen. I tillegg kan vi også bruke funksjonen `random` for å generere et tilfeldig tall med en bestemt tilfeldighetsfordeling.

## Se også

- [Offisiell Haskell dokumentasjon for tilfeldige tall](https://www.haskell.org/haskellwiki/Random_number_generation)
- [Enkel guide for å generere tilfeldige tall i Haskell](https://kunigami.blog/2014/01/21/how-to-generate-random-numbers-in-haskell/)
- [Eksperimentere med tilfeldige tall i Haskell](https://www.haskell.org/haskellwiki/Introduction_to_Haskell_IO/Common_monads#Random_numbers)