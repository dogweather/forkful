---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:36.590957-07:00
description: "Sprawdzanie, czy katalog istnieje, to podstawowa operacja w wielu zadaniach\
  \ programistycznych, pozwalaj\u0105ca na warunkowe dzia\u0142ania w oparciu o obecno\u015B\
  \u0107 lub\u2026"
lastmod: '2024-03-13T22:44:35.467163-06:00'
model: gpt-4-0125-preview
summary: "Sprawdzanie, czy katalog istnieje, to podstawowa operacja w wielu zadaniach\
  \ programistycznych, pozwalaj\u0105ca na warunkowe dzia\u0142ania w oparciu o obecno\u015B\
  \u0107 lub brak struktur katalog\xF3w."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
Haskell, za pośrednictwem swojej biblioteki bazowej, oferuje proste sposoby na sprawdzenie istnienia katalogu, głównie za pomocą modułu `System.Directory`. Spójrzmy na podstawowy przykład:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/ścieżka/do/twojego/katalogu"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Czy katalog istnieje? " ++ show exists
```

Przykładowe wyjście, w zależności od tego, czy katalog istnieje:

```
Czy katalog istnieje? True
```
Lub:
```
Czy katalog istnieje? False
```

W bardziej skomplikowanych scenariuszach lub dla dodatkowej funkcjonalności, można rozważyć popularną bibliotekę stron trzecich, jak `filepath`, do obsługi i manipulacji ścieżkami plików w bardziej abstrakcyjny sposób. Jednak w celu po prostu sprawdzenia, czy katalog istnieje, biblioteka bazowa `System.Directory` jest wystarczająca i wydajna.

Pamiętaj, że praca z systemami plików może się różnić w zależności od platform, a podejście Haskell mierzy do abstrahowania niektórych z tych różnic. Zawsze testuj operacje na plikach na docelowym systemie, aby zapewnić oczekiwane zachowanie.
