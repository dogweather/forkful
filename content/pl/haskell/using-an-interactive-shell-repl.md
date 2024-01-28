---
title:                "Korzystanie z interaktywnego shella (REPL)"
date:                  2024-01-26T04:15:06.923329-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

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
