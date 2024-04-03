---
date: 2024-01-26 04:14:50.571429-07:00
description: "Et interaktivt skall, eller REPL (Read-Eval-Print Loop), i Haskell lar\
  \ deg kj\xF8re kodefragmenter live. Det er en lekeplass for rask tilbakemelding,\
  \ testing\u2026"
lastmod: '2024-03-13T22:44:40.843333-06:00'
model: gpt-4-0125-preview
summary: "Et interaktivt skall, eller REPL (Read-Eval-Print Loop), i Haskell lar deg\
  \ kj\xF8re kodefragmenter live."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hvordan:
For å starte GHCi (Glasgow Haskell Compiler's interaktive miljø), skriv ganske enkelt `ghci` i terminalen din. Her er hvordan du bruker det:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Eksempelutdata forklarer at `x` er en numerisk variabel og viser at å doble den resulterer i 10.

## Dypdykk:
Haskells GHCi har kommet langt siden begynnelsen. Det tilbyr et rikt sett med funksjoner som tabulatorkompletering, flerlinjeinntasting og pakkelasting. Alternativer som Hugs er stort sett historiske nå, med GHCi som standarden. GHCi kompilerer kode akkurat i tide hver gang du angir et uttrykk, og gir deg en effektiv måte å teste din Haskell-kode på.

## Se også:
- [GHC-brukerveiledningen – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Lær deg en Haskell for stor suksess! – Å starte](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
