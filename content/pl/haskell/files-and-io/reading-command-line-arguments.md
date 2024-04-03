---
date: 2024-01-20 17:56:15.590997-07:00
description: "Czytanie argument\xF3w z linii polece\u0144 to proces pobierania danych\
  \ od u\u017Cytkownika, kiedy uruchamia program. Programi\u015Bci robi\u0105 to,\
  \ aby ich aplikacje mog\u0142y by\u0107\u2026"
lastmod: '2024-03-13T22:44:35.468201-06:00'
model: gpt-4-1106-preview
summary: "Czytanie argument\xF3w z linii polece\u0144 to proces pobierania danych\
  \ od u\u017Cytkownika, kiedy uruchamia program."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## Jak to zrobić:
W Haskellu, argumenty linii poleceń odczytujemy przy pomocy funkcji `getArgs` z modułu `System.Environment`. Oto jak to wygląda w praktyce:

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
```

A teraz przykładowe wywołanie i wynik:
```
$ runhaskell args.hs jeden dwa trzy
["jeden","dwa","trzy"]
```

Możesz też użyć funkcji `getProgName`, aby pobrać nazwę programu:

```Haskell
import System.Environment (getProgName, getArgs)

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    putStrLn $ "Program: " ++ progName
    putStrLn "Argumenty:"
    mapM_ putStrLn args
```

Przykładowy wynik:
```
$ runhaskell args.hs jeden dwa trzy
Program: args.hs
Argumenty:
jeden
dwa
trzy
```

## Głębsze spojrzenie:
Historia: Argumenty z linii poleceń są używane od początków informatyki. To podstawowa metoda interakcji z programami działającymi w terminalu.

Alternatywy: Oprócz `getArgs`, możesz używać bibliotek takich jak `optparse-applicative` do bardziej zaawansowanego parsowania argumentów i flag.

Szczegóły implementacyjne: `getArgs` zwraca listę argumentów jako `[String]`. Nie martw się o typy danych — Haskell zajmie się tym. Warto jednak przetwarzać te argumenty dalej, aby sprawnie obsłużyć różne opcje programu.

## Zobacz również:
- Dokumentację Haskella dla `System.Environment`: http://hackage.haskell.org/package/base-4.16.0.0/docs/System-Environment.html
- Tutorial dotyczący `optparse-applicative`: https://haskell-servant.readthedocs.io/en/stable/cookbook/using-optparse-generic/UsingOptparseGeneric.html
- Artykuł o obsłudze argumentów linii poleceń w Haskellu: https://wiki.haskell.org/Command_line_option_parsers
