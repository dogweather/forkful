---
date: 2024-01-26 04:14:55.960533-07:00
description: "Hur: F\xF6r att starta GHCi (Glasgow Haskell Compiler's interaktiva\
  \ milj\xF6), skriv helt enkelt `ghci` i din terminal. S\xE5 h\xE4r anv\xE4nder du\
  \ det."
lastmod: '2024-03-13T22:44:37.956700-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att starta GHCi (Glasgow Haskell Compiler's interaktiva milj\xF6\
  ), skriv helt enkelt `ghci` i din terminal."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur:
För att starta GHCi (Glasgow Haskell Compiler's interaktiva miljö), skriv helt enkelt `ghci` i din terminal. Så här använder du det:

```Haskell
Prelude> låt x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Exempelutmatningen förklarar att `x` är en numerisk variabel och visar att fördubbling av den resulterar i 10.

## Djupdykning:
Haskells GHCi har kommit långt sedan sin början. Det erbjuder en rik uppsättning funktioner som flikkomplettering, flerledsinmatning och paketladdning. Alternativ som Hugs är mestadels historiska nu, med GHCi som standard. GHCi kompilerar kod i realtid varje gång du matar in ett uttryck, vilket ger dig ett effektivt sätt att testa din Haskell-kod.

## Se även:
- [Användarguiden för GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Lär dig Haskell för stor nytta! – Att börja](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
