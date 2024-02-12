---
title:                "Väliaikaistiedoston luominen"
aliases:
- /fi/haskell/creating-a-temporary-file/
date:                  2024-01-20T17:40:30.297396-07:00
model:                 gpt-4-1106-preview
simple_title:         "Väliaikaistiedoston luominen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä & Miksi?
Luodaan väliaikainen tiedosto vastaanottamaan dataa, joka ei tarvitse pysyväistä tallennusta. Käytetään testauksessa, väliaikaisessa datan säilytyksessä ja tilanteissa, missä data muuttuu nopeasti.

## How to:
Miten:
Haskell antaa käteviä kirjastoja väliaikaisten tiedostojen käsittelyyn, kuten `temporary`. Tässä esimerkki sen käytöstä:

```Haskell
import System.IO
import System.IO.Temp

main :: IO ()
main = withSystemTempFile "tempfile.txt" $ \tempFilePath tempFileHandle -> do
  -- Käytä temporary-tiedostoa tempFileHandle kautta
  hPutStrLn tempFileHandle "Tämä on väliaikainen tiedosto"
  -- Luet tiedoston sisältö
  hSeek tempFileHandle AbsoluteSeek 0
  content <- hGetContents tempFileHandle
  putStrLn content
  -- Tiedosto poistetaan automaattisesti 
```

Tuloste:
```
Tämä on väliaikainen tiedosto
```

## Deep Dive:
Syväsukellus:
Haskellissa väliaikaisten tiedostojen käsittely juontaa juurensa UNIX-järjestelmien perinteisiin. `temporary`-kirjaston käyttö on suosittu lähestymistapa, mutta vaihtoehtojakin on. Voisit esim. käyttää alhaisen tason POSIX-kutsuja tai `base`-paketin `System.IO`-moduulia. `withSystemTempFile` siivoaa itse itsensä, jolloin sinun ei tarvitse huolehtia väliaikaisten tiedostojen poistamisesta.

## See Also:
Katso Myös:
- `temporary` dokumentaatio: https://hackage.haskell.org/package/temporary
- System.IO.Temp moduulin GTK-kirjaston dokumentaatio: https://hackage.haskell.org/package/base/docs/System-IO-Temp.html
- Blogi väliaikaisten tiedostojen turvallisesta käytöstä Haskellissa: [linkki tarkkaan blogiin]
- Haskellin IO-tutoriaali: http://learnyouahaskell.com/input-and-output

Muista, että sivustot ja dokumentaatiot voivat muuttua, joten tarkista aina ajantasaisuus ennen käyttöä.
