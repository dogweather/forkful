---
title:                "Odczytywanie parametrów wiersza poleceń."
html_title:           "Haskell: Odczytywanie parametrów wiersza poleceń."
simple_title:         "Odczytywanie parametrów wiersza poleceń."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Odczytywanie argumentów wiersza poleceń to proces, w którym programista pobiera wartości przekazane do programu podczas jego uruchamiania. Dzieje się to poprzez wywołanie programu z dodatkowymi parametrami, takimi jak argumenty czy opcje. Programiści często korzystają z tej funkcji, aby możliwe było dostosowanie działania programu lub podać dodatkowe informacje potrzebne do jego wykonania.

## Jak to zrobić?

```Haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Ilość argumentów: " ++ show (length args))
  putStrLn ("Argumenty: " ++ show args)
```

Przykładowy test:

```
$ runhaskell args.hs arg1 "argument drugi" 3
Ilość argumentów: 3
Argumenty: ["arg1","argument drugi","3"]
```

## Wnikliwe spojrzenie

Odczytywanie argumentów wiersza poleceń jest ważnym aspektem programowania. Począwszy od lat 60., kiedy to oprogramowanie zaczęło być uruchamiane z konsoli, możliwość przekazywania parametrów stała się kluczowa. Alternatywą dla odczytywania argumentów wiersza poleceń jest użycie plików konfiguracyjnych lub interakcja z użytkownikiem w czasie pracy programu. W języku Haskell, można wykorzystać bibliotekę System.Environment do przechwytywania argumentów wiersza poleceń. Funkcja getArgs zwraca listę argumentów przekazanych do programu jako ciągi tekstowe.

## Zobacz też

- [Dokumentacja Haskell do System.Environment](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
- [Poradnik odczytywania argumentów wiersza poleceń w Haskell](https://wiki.haskell.org/Understanding_input_and_output)
- [Przykłady kodu wykorzystującego bibliotekę System.Environment](https://github.com/haskell-streamly/haskell-streamly/blob/master/examples/args.hs)