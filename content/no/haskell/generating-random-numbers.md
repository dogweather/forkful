---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:26.324387-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall er prosessen med å lage numeriske verdier som ikke følger noe forutsigbart mønster. Programmerere bruker dette for simuleringer, spill, sikkerhetssystemer, og der hvor vi trenger å tilføre uforutsigbarhet.

## Slik gjør du:
Haskell håndterer tilfeldige tall litt annerledes enn andre språk. Du trenger `random`-biblioteket. Installer med `cabal install random`. Her er et enkelt eksempel:

```Haskell
import System.Random

main :: IO ()
main = do
    g <- newStdGen
    let (randNumber, _) = random g :: (Int, StdGen)
    print randNumber
```

Kjører du dette, får du et tilfeldig heltall hver gang.

For å generere et tilfeldig tall mellom 0 og 100:

```Haskell
import System.Random

main :: IO ()
main = do
    g <- newStdGen
    let (randNumber, _) = randomR (0, 100) g :: (Int, StdGen)
    print randNumber
```

## Dypdykk
I Haskell er det ikke som i imperativespråk hvor du bare ber om et nytt tall. Haskell er funksjonell og ren, så den gjør det gjennom ren funksjonell tilstand. Det betyr at tilfeldige tall genereres via en 'seed', og bruker denne til å skape en serie. Dermed må du frakte 'seed'-verdien (ofte kalt `StdGen` i Haskell) rundt hvis du trenger mer enn ett tall.

Historisk sett har tilfeldige tallgeneratorer utviklet seg fra enkle matematiske algoritmer til mer komplekse. Haskell bruker `StdGen`, en vitenskapelig og statistisk robust generator.

Det finnes alternativer som `System.Random.MWC` eller `System.Random.PCG` for bedre ytelse eller egenskaper.

Implementasjonsdetaljer kan bli komplekse, men det essensielle er at `random` og `randomR` funksjonene håndterer kompleksiteten for deg. Bare husk å behandle `StdGen`-objektet forsiktig og kun bruke det én gang per tilfeldig tall.

## Se Også
- [Haskell Library - System.Random](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [For en innføring i mer avanserte tilfeldige tallgenereringsbiblioteker.](https://www.stackage.org/lts/package/mwc-random)
