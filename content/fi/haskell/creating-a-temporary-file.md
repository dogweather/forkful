---
title:    "Haskell: Väliaikaisen tiedoston luominen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary-tiedostojen luominen on tärkeä osa Haskell-ohjelmoinnissa. Se tarjoaa tehokkaan ja luotettavan tavan tallentaa ja käyttää väliaikaista dataa ohjelman suorituksen aikana.

## Miten

### Luodaan väliaikainen tiedosto

```Haskell
import System.IO.Temp

main = do
  -- Luodaan väliaikainen tiedosto oletuskansioon
  withSystemTempFile "tempfile.txt" $ \tmpPath handle -> do
    -- Kirjoitetaan data tiedostoon
    hPutStrLn handle "Tämä on väliaikainen tiedosto"
    -- Suljetaan tiedosto
    hClose handle
    -- Tulostetaan tiedoston polku
    putStrLn $ "Luotu tiedosto: " ++ tmpPath
```

### luoEmbeddedFile-funktion käyttö

```Haskell
import System.IO.Temp
import Data.ByteString.Char8

main = do
  -- Tallennetaan väliaikainen data muuttujaan
  let data = "Tämä on väliaikainen tiedosto"
  -- Luodaan väliaikainen tiedosto siirtämällä data luoEmbeddedFile-funktiolle
  (tmpPath, _) <- createTempFile Nothing "tempfile.txt"
                      (folderPath, pack data)
  -- Tulostetaan tiedoston polku
  putStrLn $ "Luotu tiedosto: " ++ tmpPath
```

### Väliaikaisen tiedoston poistaminen

```Haskell
import System.IO.Temp
import System.Directory

main = do
  -- Luodaan väliaikainen tiedosto
  (tmpPath, handle) <- openTempFile "folder" "tempfile.txt"
  -- Kirjoitetaan data tiedostoon
  hPutStrLn handle "Tämä on väliaikainen tiedosto"
  -- Suljetaan tiedosto
  hClose handle
  -- Poistetaan tiedosto
  removeFile tmpPath
```

## Syväsukellus

Väliaikaisten tiedostojen luominen on tärkeä osa Haskell-ohjelmia, jotka käsittelevät suuria määriä dataa tai tarvitsevat väliaikaisia tallennuspaikkoja väliaikaisille tiedostoille.

Suorituskyvyn optimoimiseksi on suositeltavaa käyttää ```createTempFile``` tai ```withSystemTempFile``` -funktiota, joka käyttää käyttöjärjestelmän osoittamaa väliaikaista hakemistoa tiedostojen tallentamiseen. Tällä tavalla voit varmistaa, että väliaikaiset tiedostot hävitetään lopuksi automaattisesti.

## Katso myös
- [Haskellin dokumentaatio System.IO.Temp-moduulista](https://www.haskell.org/cabal/users-guide/developing-packages.html#system-temporary-files)
- [Haskelin dokumentaatio Data.ByteString.Char8-moduulista](https://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString-Char8.html)
- [Haskelin dokumentaatio System.Directory-moduulista](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)