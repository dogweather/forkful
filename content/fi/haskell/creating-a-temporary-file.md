---
title:                "Haskell: Luodaan tilapäistiedosto"
simple_title:         "Luodaan tilapäistiedosto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Joskus Haskell-ohjelmissa voi olla tarvetta luoda väliaikaisia tiedostoja. Ne voivat olla tarpeellisia esimerkiksi datan tallentamiseen tai väliaikaiseen tiedonkäsittelyyn. Tässä blogipostissa opit, miten luodaan väliaikaisia tiedostoja Haskellissa.

## Miten

```Haskell
import System.IO.Temp
import System.Directory

main = do
  -- Luodaan väliaikainen tiedosto
  tempFile <- openTempFile "kansio" "tiedosto.txt" 
  writeFile (fst tempFile) "Tämä on väliaikainen tiedosto"
  hClose (snd tempFile)

  -- Luodaan väliaikainen hakemisto
  tempDir <- createTempDirectory "kansio" "hakemisto" 
  putStrLn tempDir
```

```haskell
-- Ohjelman tulostama output:
"kansio31751UT/tiedosto.txt"
"kansio78450UT/hakemisto"
```

Väliaikainen tiedosto luodaan `openTempFile` -funktiolla, joka ottaa argumentteina polun ja tiedostonimen. `fst` -funktio palauttaa tiedostoparin ensimmäisen osan, eli tiedoston polun. `snd` -funktio puolestaan palauttaa tiedostoparin toisen osan, eli tiedoston kahvan. Näiden avulla voimme käsitellä tiedostoa esimerkiksi `writeFile` ja `hClose` -funktioiden avulla.

Väliaikainen hakemisto luodaan `createTempDirectory` -funktiolla, joka ottaa samat argumentit kuin `openTempFile`. `putStrLn` tulostaa luodun hakemiston polun.

## Syvempää tietoa

Haskellin `System.IO.Temp` -paketti tarjoaa paljon enemmän mahdollisuuksia väliaikaisten tiedostojen luomiseen, kuten automaattisen nimien generoinnin, paketoinnin ja uudelleennimeämisen. Voit tutustua niihin tarkemmin [dokumentaatiosta](https://hackage.haskell.org/package/temp-1.2.3/docs/System-IO-Temp.html).

## Katso myös

- [Temporary files and directories in Haskell](https://ro-che.info/articles/2014-01-16-temporary-files-and-directories-in-haskell)
- [Haskell System.Directory - Temporary files and directories](https://hackage.haskell.org/package/directory-1.2.3.1/docs/System-Directory.html#t:FilePath)
- [Creating temporary files in Haskell](https://blog.wuzi.io/creating-temporary-files-in-haskell/)