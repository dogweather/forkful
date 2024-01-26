---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:54:20.714393-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstin lukeminen tiedostosta on yksinkertaisesti tiedon hakemista levyltä. Ohjelmoijat tekevät tätä datan käsittelyn, analysoinnin tai tulosten tallentamisen vuoksi.

## How to:
```haskell
import System.IO

-- Tiedoston lukeminen rivillesi
main = do
    fileHandle <- openFile "esimerkki.txt" ReadMode
    contents <- hGetContents fileHandle
    putStr contents
    hClose fileHandle

-- Toisaalta, käytämme 'readFile' funktiota (helppo tapa):
mainLueHelppo = do
    contents <- readFile "esimerkki.txt"
    putStr contents

-- Yksinkertainen tapa luetella tiedoston rivit:
mainRivit = do
    contents <- readFile "esimerkki.txt"
    let rivit = lines contents
    mapM_ putStrLn rivit
```
Odotettu tuloste:
```
Moi, tämä on tiedosto esimerkki.
Tässä on toinen rivi.
```

## Deep Dive
Tekstitiedostojen käsittely on oleellista monille sovelluksille. Historiassa tiedostonkäsittelyyn käytettiin alhaisen tason IO-operaatioita C:ssä. Haskellissa se yksinkertaistui funktioiden, kuten `readFile` ja `hGetContents`, myötä.

Vaihtoehtoisesti voit käyttää `ByteString` tai `Text` kirjastoa, jotka käsittävät suuren datamäärän tehokkaammin kuin perinteiset `String`:it.

Kun avaat tiedoston, muista aina sulkea se. Tämä vapauttaa resurssit. `withFile` funktio hoitaa tämän automaattisesti.

## See Also
- [Real World Haskell, Chapter 7](http://book.realworldhaskell.org/read/io.html)
- [Hackage: ByteString package](https://hackage.haskell.org/package/bytestring)
