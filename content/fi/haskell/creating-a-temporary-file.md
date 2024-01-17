---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Haskell: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Luodessa ohjelmia, saattaa joskus olla tarpeen luoda väliaikaisia tiedostoja. Tämä voi tapahtua esimerkiksi silloin, kun ohjelmalle tarvitaan tallennustila väliaikaisille tiedoille tai kun halutaan kopioida tiedostoja ennen niiden muokkaamista. Väliaikaiset tiedostot ovat siis vain väliaikaisessa käytössä ja poistetaan yleensä lopuksi.

## Miten:
Haskell-ohjelmassa väliaikaisen tiedoston luominen tapahtuu Functools-moduulin avulla käyttäen `withTempFile`-funktiota. Tämä avaa uuden väliaikaisen tiedoston ja palauttaa sen tiedostonimen sekä avaamisen yhteydessä luodun kahva-objektin. Tässä esimerkki koodista:

```Haskell
import System.IO.Temp (withTempFile)

main = withTempFile "temporary.txt" $ \tempFilePath tempFileHandle -> do
    putStrLn $ "Luotiin väliaikainen tiedosto: " ++ tempFilePath
    hPutStrLn tempFileHandle "Tässä on väliaikaisen tiedoston sisältö."
```

Tämän koodin suorittamisen jälkeen prosessin juurikansioon luodaan tiedosto nimeltä `temporary.txt`, joka sisältää tekstirivin "Tässä on väliaikaisen tiedoston sisältö.".

## Syvempi sukellus:
Väliaikaisen tiedoston luomisesta on monta eri tapaa. Ennen `withTempFile`-funktion lisäämistä Functools-moduuliin, väliaikaisia tiedostoja luotiin yleensä system-kutsulla. Tässä on esimerkki koodista, joka käyttää system-kutsua luomaan väliaikaisen tiedoston:

```Haskell
import System.Process

main = do
    (tempFilePath, tempFileHandle) <- readProcess "mktemp" ["-q", "temporaryXXXXXX.txt"] ""
    putStrLn $ "Luotiin väliaikainen tiedosto: " ++ tempFilePath
    writeFile tempFilePath "Tässä on väliaikaisen tiedoston sisältö."
```

On hyvä huomata, että `withTempFile`-funktio huolehtii automaattisesti väliaikaisen tiedoston poistamisesta, kun taas system-kutsua käytettäessä tämä tulee tehdä itse koodin avulla.

## Katso myös:
- [Haskellin Functools-moduulin dokumentaatio](https://hackage.haskell.org/package/functools/docs/System-IO-Temp.html)
- [Haskellin system-kutsun dokumentaatio](https://hackage.haskell.org/package/process/docs/System-Process.html)