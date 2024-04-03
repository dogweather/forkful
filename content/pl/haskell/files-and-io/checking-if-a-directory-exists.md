---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:36.590957-07:00
description: "Jak to zrobi\u0107: Haskell, za po\u015Brednictwem swojej biblioteki\
  \ bazowej, oferuje proste sposoby na sprawdzenie istnienia katalogu, g\u0142\xF3\
  wnie za pomoc\u0105 modu\u0142u\u2026"
lastmod: '2024-03-13T22:44:35.467163-06:00'
model: gpt-4-0125-preview
summary: "Haskell, za po\u015Brednictwem swojej biblioteki bazowej, oferuje proste\
  \ sposoby na sprawdzenie istnienia katalogu, g\u0142\xF3wnie za pomoc\u0105 modu\u0142\
  u `System.Directory`."
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
