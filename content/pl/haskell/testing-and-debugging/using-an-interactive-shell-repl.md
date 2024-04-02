---
date: 2024-01-26 04:15:06.923329-07:00
description: "Interaktywna pow\u0142oka, czyli REPL (Read-Eval-Print Loop), w Haskellu\
  \ pozwala na uruchamianie fragment\xF3w kodu na \u017Cywo. Jest to plac zabaw dla\
  \ szybkiego\u2026"
lastmod: '2024-03-13T22:44:35.453489-06:00'
model: gpt-4-0125-preview
summary: "Interaktywna pow\u0142oka, czyli REPL (Read-Eval-Print Loop), w Haskellu\
  \ pozwala na uruchamianie fragment\xF3w kodu na \u017Cywo. Jest to plac zabaw dla\
  \ szybkiego\u2026"
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Co i dlaczego?
Interaktywna powłoka, czyli REPL (Read-Eval-Print Loop), w Haskellu pozwala na uruchamianie fragmentów kodu na żywo. Jest to plac zabaw dla szybkiego uzyskiwania informacji zwrotnych, testowania funkcji i nauki języka.

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
