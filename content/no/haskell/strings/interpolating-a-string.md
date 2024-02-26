---
date: 2024-01-20 17:51:06.431996-07:00
description: "Strenginterpolering lar deg sette inn variabler eller uttrykk inni en\
  \ tekststreng. Det gj\xF8r det enklere \xE5 bygge dynamiske tekster, og holder koden\
  \ ryddig\u2026"
lastmod: '2024-02-25T18:49:38.999661-07:00'
model: gpt-4-1106-preview
summary: "Strenginterpolering lar deg sette inn variabler eller uttrykk inni en tekststreng.\
  \ Det gj\xF8r det enklere \xE5 bygge dynamiske tekster, og holder koden ryddig\u2026"
title: Interpolering av en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Strenginterpolering lar deg sette inn variabler eller uttrykk inni en tekststreng. Det gjør det enklere å bygge dynamiske tekster, og holder koden ryddig og leselig.

## Hvordan:

Interpolering i Haskell kan gjøres med `printf` fra `Text.Printf` biblioteket eller med `interpolate` fra `Data.String.Interpolate` for å få en mer moderne syntaks à la template strings i JavaScript.

```Haskell
import Text.Printf

main :: IO ()
main = do
   let navn = "Verden"
   putStrLn (printf "Hei, %s!" navn)
```
Output:
```
Hei, Verden!
```

Eller ved hjelp av `interpolate` bibilioteket:

```Haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate ( i )

main :: IO ()
main = do
   let navn = "Verden"
   putStrLn [i|Hei, #{navn}!|]
```
Output:
```
Hei, Verden!
```

## Deep Dive

Tidligere i Haskell, var strengkonkatenering og `++` operatoren normen for å bygge strenger med variabler. Men det var tungvint og feilutsatt. `printf`-stilen kom fra C og ble adoptert i Haskell for tradisjonell strengformatering. Den fungerer, men syntaksen er ikke ideell.

`Data.String.Interpolate` er et tredjepartsbibliotek som introduserer moderne strenginterpolering. Dette biblioteket bruker en quasiquote syntaks (`[i|...|]`), som gir mer lesbar kode og direkte støtte for interpolering uten å måtte bruke spesielle tegn eller formatteringsdirektiver som med `printf`.

For å implementere strenginterpolering på denne måten, bruker `Data.String.Interpolate` Haskell sin Template Haskell-funksjonalitet, som er en kraftig metaprogrammeringsfasilitet. Det oversetter interpolerte strenger til effektiv konkatenert kode ved kompileringstid, og sikrer god ytelse.

## See Also

For videre lesning og dybdekunnskap, sjekk ut disse ressursene:
- [Haskell's Text.Printf documentation](http://hackage.haskell.org/package/base-4.14.0.0/docs/Text-Printf.html)
- [Data.String.Interpolate on Hackage](http://hackage.haskell.org/package/interpolate)
- [Haskell Wiki: String interpolation](https://wiki.haskell.org/String_interpolation)
