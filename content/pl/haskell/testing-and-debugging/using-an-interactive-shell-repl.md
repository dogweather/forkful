---
date: 2024-01-26 04:15:06.923329-07:00
description: "Jak to zrobi\u0107: Aby uruchomi\u0107 GHCi (interaktywne \u015Brodowisko\
  \ Glasgow Haskell Compiler), wystarczy wpisa\u0107 `ghci` w terminalu. Oto jak z\
  \ niego korzysta\u0107."
lastmod: '2024-03-13T22:44:35.453489-06:00'
model: gpt-4-0125-preview
summary: "Aby uruchomi\u0107 GHCi (interaktywne \u015Brodowisko Glasgow Haskell Compiler),\
  \ wystarczy wpisa\u0107 `ghci` w terminalu."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak to zrobić:
Aby uruchomić GHCi (interaktywne środowisko Glasgow Haskell Compiler), wystarczy wpisać `ghci` w terminalu. Oto jak z niego korzystać:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Przykładowe wyjście wyjaśnia, że `x` jest zmienną numeryczną i pokazuje, że podwojenie jej daje wynik 10.

## Zagłębienie się:
GHCi w Haskellu przebył długą drogę od swojego powstania. Oferuje bogaty zestaw funkcji, takich jak autouzupełnianie, wieloliniowe wejście i ładowanie pakietów. Alternatywy, takie jak Hugs, są obecnie głównie historyczne, z GHCi będącym standardem. GHCi kompiluje kod na bieżąco za każdym razem, gdy wprowadzasz wyrażenie, dając efektywny sposób na testowanie kodu Haskell.

## Zobacz również:
- [Przewodnik użytkownika GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – Rozpoczęcie](http://learnyouahaskell.com/starting-out#hello-world)
- [Wiki Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
