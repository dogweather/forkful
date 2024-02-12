---
title:                "Å jobbe med komplekse tall"
aliases:
- /no/haskell/working-with-complex-numbers.md
date:                  2024-01-26T04:41:47.812184-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med komplekse tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Komplekse tall, som består av en reell og en imaginær del, er essensielle i ulike beregningsfelt som ingeniørvitenskap, fysikk og signalbehandling. Programmerere bruker dem til å løse ligninger som reelle tall ikke kan, som å finne røttene til negative tall.

## Hvordan:

Haskell håndterer komplekse tall med modulen `Data.Complex`. Her er en rask gjennomgang:

```haskell
import Data.Complex

-- Definer to komplekse tall
let z1 = 3 :+ 4  -- det er 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- Aritmetiske operasjoner
let sum = z1 + z2  -- 8 :+ 2
let differanse = z1 - z2  -- -2 :+ 6
let produkt = z1 * z2  -- 23 :+ 14
let kvotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- Kompleks konjugering
let konjugertZ1 = conjugate z1  -- 3 :+ (-4)

-- Størrelse og fase
let størrelseZ1 = magnitude z1  -- 5.0
let faseZ1 = phase z1  -- 0.9272952180016122

-- Omforming fra polar til rektangulær og omvendt
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fraPolar = mkPolar 5.0 0.9272952180016122  -- samme som z1
```

Eksempel på utdata etter å ha lastet inn koden ovenfor i GHCi kan være:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> produkt
23.0 :+ 14.0
*Main> størrelseZ1
5.0
```

## Dypdykk

Komplekse tall går tilbake til det 16. århundret, men ble bredt akseptert mye senere. Haskell, som mange andre språk, tilbyr innebygd støtte for kompleks aritmetikk, noe som gjør det enkelt å jobbe med disse tallene uten å måtte implementere den underliggende matematikken.

Alternativer inkluderer å bygge din egen kompleks talltype eller å bruke biblioteker for spesifikke domener som for eksempel kvaternioner for 3D-grafikk. Men for de fleste bruksområder er Haskell's `Data.Complex` nok.

Under hetten er `Data.Complex` bare en datatype som parer to `Float` eller `Double` verdier, som representerer henholdsvis den reelle og den imaginære delen. Det er en enkel og effektiv måte å jobbe med komplekse tall på Haskell-plattformen.

## Se Også

Sjekk ut disse ressursene for mer om komplekse tall i Haskell:

- Den offisielle Haskell `Data.Complex` dokumentasjonen: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Et dypere dykk inn i Haskell's talltyper: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- For en anvendelse, utforsk algoritmer for rask Fourier-transformasjon i Haskell: [Haskell FFT-bibliotek](https://hackage.haskell.org/package/fft)
