---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:56:26.752490-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja miksi?
Tarkistetaan, onko hakemisto olemassa, eli testataan tiedostojärjestelmässä kyseisen polun olemassaolo. Ohjelmoijat tekevät tämän välttääkseen virheitä kuten tiedoston kirjoittamisen olemattomaan hakemistoon.

## How to: - Kuinka tehdä:
```haskell
import System.Directory (doesDirectoryExist)

-- Pääfunktio, joka käyttää tarkistusta
main :: IO ()
main = do
    let dirPath = "/path/to/directory"
    exists <- doesDirectoryExist dirPath
    putStrLn $ "Directory " ++ dirPath ++ (if exists then " exists." else " does not exist.")

-- Esimerkkinä voisi olla:
-- Directory /path/to/directory exists.
```

## Deep Dive - Syväsukellus:
Haskell käyttää `System.Directory` -moduulia hakemistojen tarkistamiseen. Funktio `doesDirectoryExist` tuli käyttöön GHC:n version 6.2.2 mukana. Vaihtoehtoisesti voi käyttää `System.FilePath` yhdistettynä käyttöjärjestelmäkohtaisiin toimintoihin, kuten `posix` ja `unix`, mutta `doesDirectoryExist` on yksinkertaisin ja idiomaattisin tapa Haskellissa. Funktion toteutus käyttää alhaisen tason API-kutsuja tiedostojärjestelmän tilan selvittämiseen.

## See Also - Katso myös:
- Haskell `System.Directory` dokumentaatio: https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
- `System.FilePath` moduulin dokumentaatio: https://hackage.haskell.org/package/filepath
- Virallinen oppaasi Haskell-ohjelmointikieleen: https://www.haskell.org/documentation
