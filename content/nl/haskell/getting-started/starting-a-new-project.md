---
changelog:
- 2024-01-21, dogweather, Reviewed for accuracy
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:15.634438-07:00
description: "Hoe te: Haskell-projecten vertrouwen vaak op tools zoals Stack of Cabal.\
  \ Stack beheert afhankelijkheden, wat zorgt voor consistente builds. In 2008 was\u2026"
lastmod: '2024-04-05T21:53:50.877120-06:00'
model: gpt-4-0125-preview
summary: Haskell-projecten vertrouwen vaak op tools zoals Stack of Cabal.
title: Een nieuw project starten
weight: 1
---

## Hoe te:
```Haskell
-- 1. Een nieuw Haskell-project initialiseren met Stack
$ stack new myproject

-- Het bovenstaande commando maakt een nieuwe map `myproject` met enkele bestanden:
-- myproject/
-- ├── app/
-- │   └── Main.hs        # Je hoofdapplicatiebestand
-- ├── src/               # Bronbestanden voor de bibliotheek
-- ├── test/              # Testbestanden
-- ├── myproject.cabal    # Pakketbeschrijving bestand
-- ├── stack.yaml         # Stack configuratie
-- └── Setup.hs           # Bouwsetup script

-- 2. Het project bouwen
$ cd myproject
$ stack build

-- 3. Je nieuwe Haskell-project uitvoeren
$ stack run

-- Voorbeelduitvoer:
someFunc
```

## Diepe Duik
Haskell-projecten vertrouwen vaak op tools zoals Stack of Cabal. Stack beheert afhankelijkheden, wat zorgt voor consistente builds. In 2008 was Stack een gamechanger voor Haskell, wat de tekortkomingen van Cabal met pakketconflicten aanpakte.

Alternatieven zijn het gebruik van alleen Cabal of nieuwere tools zoals GHCup of Nix voor reproduceerbare builds. Je zou kunnen kiezen voor Cabal voor de eenvoud of Nix wanneer je werk reproduceerbaarheid vereist, maar Stack vindt een gelukkige balans voor velen.

Onder de motorkap maakt `stack new` gebruik van een sjabloon om een project op te zetten. Het omvat niet alleen je broncode, maar ook configuraties voor het bouwen en afhankelijkheden. Het `.cabal` bestand is cruciaal, bevat metadata en bouwinstructies.

## Zie Ook
- Leer meer over Stack: [De Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- Duik in Cabal: [De Haskell Cabal](https://www.haskell.org/cabal/users-guide/)
