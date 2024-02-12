---
title:                "Sprawdzanie, czy katalog istnieje"
aliases: - /pl/haskell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:36.590957-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje, to podstawowa operacja w wielu zadaniach programistycznych, pozwalająca na warunkowe działania w oparciu o obecność lub brak struktur katalogów. Jest to kluczowe dla manipulacji plikami, automatycznych skryptów oraz podczas początkowej konfiguracji oprogramowania, aby zapewnić, że niezbędne katalogi są na miejscu, lub aby uniknąć duplikacji katalogów.

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
