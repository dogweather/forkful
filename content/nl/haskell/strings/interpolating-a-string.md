---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:18.044446-07:00
description: "Hoe te: In Haskell is stringinterpolatie niet standaard ingebouwd, maar\
  \ met het `interpolate` pakket kun je er vrij dichtbij komen. Zorg eerst dat je\
  \ het\u2026"
lastmod: '2024-03-13T22:44:50.838384-06:00'
model: gpt-4-0125-preview
summary: In Haskell is stringinterpolatie niet standaard ingebouwd, maar met het `interpolate`
  pakket kun je er vrij dichtbij komen.
title: Een string interpoleren
weight: 8
---

## Hoe te:
In Haskell is stringinterpolatie niet standaard ingebouwd, maar met het `interpolate` pakket kun je er vrij dichtbij komen. Zorg eerst dat je het pakket hebt:

```bash
cabal update
cabal install interpolate
```

Schrijf nu wat Haskell:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
    let name = "wereld"
    let greeting = [i|Hallo, #{name}!|]
    putStrLn greeting
```

Voer het uit:

```
Hallo, wereld!
```

## Diepgaande Duik
Historisch gezien kwam Haskell niet standaard met stringinterpolatie uit de doos. Het is een functie die vaker voorkomt in scripttalen. Interpolatie in Haskell werd soepeler met de ontwikkeling van quasiquoters, die je toestaan om je eigen aangepaste syntax te definiëren—zoals onze `i` voor het interpoleren van strings.

Alternatieven? Zeker, gebruik `printf` van `Text.Printf`, of voeg strings en variabelen samen met `++`. Maar deze missen de elegantie en eenvoudigheid van interpolatie.

Wat de implementatie betreft, transformeert `interpolate` je geïnterpoleerde strings in reguliere Haskell strings op compilatietijd met behulp van Template Haskell, dus er is geen prestatievermindering bij het uitvoeren van je code. Het is slim en netjes, net als Haskell.

## Zie Ook
- [Hackage - interpolate pakket](https://hackage.haskell.org/package/interpolate)
- [Hackage - Text.Printf module](https://hackage.haskell.org/package/base/docs/Text-Printf.html)
- [Haskell Wiki - Quasiquotation](https://wiki.haskell.org/Quasiquotation)
- Voor rijke templating, bekijk [Hackage - Mustache templates](https://hackage.haskell.org/package/mustache)
