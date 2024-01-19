---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tilapäiset tiedostot ovat tiedostoja, jotka luodaan tallentamaan väliaikaista tietoa. Ohjelmoijat käyttävät niitä tallentamaan suuria määriä tietoa, jota ei tarvitse säilyttää pitkäaikaisesti.

## Näin teet:

Voimme luoda väliaikaiset tiedostot Haskellissa `System.IO.Temp` -kirjastoa käyttäen.

```Haskell
import System.IO.Temp

esimerkki = withSystemTempFile "temp.txt" $ \tempPath tempHandle -> do
  hPutStrLn tempHandle "Tämä on väliaikainen tiedosto Haskellissa"
  hClose tempHandle
  contents <- readFile tempPath
  putStrLn contents
```

Kun suoritat tämän koodin, se luo väliaikaisen tiedoston, kirjoittaa sen sisältöön, sitten lukee ja tulostaa sen sisällön. Tiedosto poistetaan automaattisesti `withSystemTempFile`-funktion suorittamisen jälkeen.

## Syvä sukellus:

Väliaikaisten tiedostojen luomisen historiassa yksi suurimmista haasteista oli turvallisuus. Haskell tarjoaa ratkaisun tähän käyttämällä ainutlaatuista tiedostonimeä joka kerta `withSystemTempFile`-funktion avulla.

Jos ei halua käyttää `System.IO.Temp`-kirjastoa, voit luoda tiedoston manuaalisesti ja hallita sen elinkaarta itse. 

```Haskell 
import System.IO 

esimerkki2 = do 
  let tempPath = "temp2.txt"
  writeFile tempPath "Toinen esimerkki väliaikaisesta tiedostosta"
  contents <- readFile tempPath
  putStrLn contents 
  removeFile tempPath
```

Tämä on hyvä vaihtoehto, jos haluat hallita itse tiedoston elinkaarta, mutta vaatii muistaa poistaa tiedosto, kun sitä ei enää tarvita.

## Katso Myös:

- Haskellin virallisista dokumentaatioista löydät lisätietoa `System.IO.Temp` -kirjastosta:
  - [System.IO.Temp](https://hackage.haskell.org/package/temporary-0.3.0.1/docs/System-IO-Temp.html)
- Lisätietoja tiedostojen kanssakäymisestä Haskellissa:
  - [Learn You a Haskell - Tiedostot ja virranhallinta](http://learnyouahaskell.com/input-and-output)