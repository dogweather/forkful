---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:56:58.803802-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Czym jest i dlaczego?)
Sprawdzanie istnienia katalogu w Haskellu to proces weryfikacji, czy katalog znajduje się w systemie plików. Programiści robią to, aby uniknąć błędów podczas próby dostępu lub modyfikacji plików w nieistniejącym katalogu.

## How to: (Jak to zrobić:)
```Haskell
import System.Directory (doesDirectoryExist)

-- Użycie funkcji doesDirectoryExist do sprawdzenia katalogu
main :: IO ()
main = do
    let dirPath = "/path/to/your/directory"
    exists <- doesDirectoryExist dirPath
    putStrLn $ "Katalog " ++ (if exists then "istnieje." else "nie istnieje.")
```
Sample output (Przykładowe wyjście):
```
Katalog istnieje.
```
lub jeśli katalogu nie ma:
```
Katalog nie istnieje.
```

## Deep Dive (Dogłębna analiza):
W przeszłości, sprawdzenie istnienia katalogu mogło wymagać bezpośredniego wywołania poleceń systemowych lub komplikacji z używaniem bibliotek systemowych języka C. W nowoczesnym Haskellu używamy modułu `System.Directory`, który udostępnia funkcje wyższego poziomu, takie jak `doesDirectoryExist`. Jest to abstrakcja, która zapewnia przenośność między różnymi systemami operacyjnymi.

Alternatywą dla `doesDirectoryExist` może być ręczne sprawdzenie przy pomocy funkcji `getDirectoryContents` czy `catch`, ale to zwykle niepotrzebne dodatkowe kroki. Natomiast implementacja funkcji `doesDirectoryExist` polega na wykorzystaniu odpowiednich wywołań systemowych zależnych od platformy – na przykład wywołania `stat` w systemach rodziny UNIX.

## See Also (Zobacz także):
- Haskell `System.Directory` documentation: [Hackage – System.Directory](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
