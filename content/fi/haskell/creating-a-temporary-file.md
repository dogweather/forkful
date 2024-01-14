---
title:                "Haskell: Tilapäistiedoston luominen"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Monet kirjoittajat ovat kohdanneet tarpeen luoda tilapäistiedostoja ohjelmointia varten. Tämä voi johtua siitä, että haluamme tallentaa tiedostoja vain tilapäisesti, käsikirjoituksen testaamiseksi tai tiedostojen luomiseksi toisista tiedoista. Tässä blogiviestissä käsittelemme, miten voit luoda tilapäistiedoston helposti Haskell-ohjelmassa.

## Kuinka

Tilapäisten tiedostojen luominen Haskell-ohjelmassa on helppoa käyttämällä moduulia `System.IO.Temp`. Seuraavaksi näytämme yksinkertaisen esimerkin tilapäistiedoston luomisesta ja sen avaamisesta tiedon kirjoittamiseksi:

```Haskell
import System.IO.Temp
import System.IO
import System.Directory

main = do
  -- Luodaan tilapäistiedosto ja avataan se ReadWrite-tilassa
  withSystemTempFile "temp.txt" $ \tempFilePath tempHandle -> do
    -- Kirjoitetaan tiedostoon
    hPutStrLn tempHandle "Tämä on tilapäistiedosto Haskellilla!"
    -- Suljetaan tiedosto
    hClose tempHandle
    -- Luetaan tiedoston sisältö ja tulostetaan se
    readHandle <- openFile tempFilePath ReadMode
    tempContents <- hGetContents readHandle
    putStrLn tempContents
    -- Poistetaan tilapäistiedosto
    removeFile tempFilePath
```

Tämän koodin tulosteena pitäisi olla: "Tämä on tilapäistiedosto Haskellilla!" Tämä esimerkki osoittaa, kuinka voit luoda tilapäistiedoston, kirjoittaa siihen ja lukea sen sisältöä. Lopuksi tiedosto poistetaan `removeFile`-funktion avulla.

## Syvällinen sukellus

Jos haluat lisätietoja tilapäistiedostojen luomisesta Haskell-ohjelmassa, lue moduulin `System.IO.Temp` dokumentaatio. Tämä moduuli tarjoaa myös muita käteviä toimintoja tilapäistiedostojen käsittelyyn, kuten `withTempDirectory`- ja `emptyTempFile`-funktiot.

## Katso myös

- [Haskellin `System.IO.Temp`-moduulin dokumentaatio](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO-Temp.html)
- [Haskelliin tutustuminen -opas (suomeksi)](https://fi.wikibooks.org/wiki/Haskell:_Tutustu_Haskelliin)