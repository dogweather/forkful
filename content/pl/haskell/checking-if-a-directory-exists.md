---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Haskell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co to jest i po co?

Sprawdzanie, czy katalog istnieje, to po prostu mechanizm, który umożliwia programom zrozumienie struktury plików na dysku. Programiści robią to, aby uniknąć błędów podczas prób odczytu lub zapisu do lokalizacji, których może nie być.

## Jak to zrobić:

```haskell
import System.Directory

sprawdzKatalog :: FilePath -> IO Bool
sprawdzKatalog = doesDirectoryExist
```
Gdy teraz wywołasz `sprawdzKatalog "./mojKatalog"`, dostaniesz `True` lub `False` w zależności od tego, czy katalog istnieje.

## Wgłębne spojrzenie

Jak sugerują stare źródła biblioteki `System.Directory`, możliwość sprawdzania istnienia katalogów jest dostępna od czasów Haskell 98, co podkreśla użyteczność i konieczność takiej funkcji. Alternatywa to używanie `getPermissions` i `readable`, ale są one mniej bezpośrednie i mogą prowadzić do wołania błędów, jeśli katalog nie istnieje.

Konkretnie, `doesDirectoryExist` zaczyna od sprawdzenia, czy ścieżka istnieje za pomocą `doesPathExist`, a następnie sprawdza, czy to jest katalog. To ważne, bo świat systemów plików jest pełen różnych przypadków. Możliwe jest na przykład, że ścieżka existuje, ale nie jest katalogiem.

## Zobacz też

* Ogólne informacje o System.Directory można znaleźć [tutaj](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html)
* Niezmiernie przydatne wprowadzenie do programowania w Haskellu znajdziesz [tutaj](http://learnyouahaskell.com/).