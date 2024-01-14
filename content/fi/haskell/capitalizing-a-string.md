---
title:                "Haskell: Merkkijonon kirjoittaminen isolla alkukirjaimella"
simple_title:         "Merkkijonon kirjoittaminen isolla alkukirjaimella"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä saattaa olla tarve muuttaa merkkijono isolla alkukirjaimella alkavaksi. Tällainen toiminto voi olla tarpeen esimerkiksi kun halutaan tulostaa käyttäjän syöttämä nimi tai kun muutetaan tietyn formaatin vaatimuksia vastaavaksi. Tässä blogipostissa käymme läpi, miten merkkijonon ensimmäinen kirjain voidaan muuttaa isoksi.

## Kuinka tehdä

Merkkijonon ensimmäisen kirjaimen muuttaminen isoksi onnistuu helposti Haskellissa käyttämällä `toUpper` funktiota. Tämä funktio kuuluu `Data.Char` moduuliin, joten se täytyy tuoda käyttöön ensin `import`-lauseella. Seuraavassa esimerkissä käydään läpi yksinkertainen tapa muuttaa merkkijonon ensimmäinen kirjain isoksi ja tulostaa tulos konsoliin.

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

main = do
  putStrLn "Syötä nimi:"
  name <- getLine
  putStrLn ("Tervehdys " ++ capitalize name)
```

Tämän esimerkin avulla pystytään muuttamaan käyttäjän syöttämä nimi isolla alkukirjaimella alkavaksi ja tulostamaan se tervehdyksen muodossa.

## Syvemmälle

Haskellissa merkkijonojen muokkaaminen tapahtuu yleensä listana merkeistä. Tästä syystä merkkijonon ensimmäisen kirjaimen muuttaminen vaatii listan käsittelyä. Esimerkissämme funktio `capitalize` ottaa parametrinaan merkkijonon ja tarkistaa ensin, ettei se ole tyhjä. Jos merkkijono on tyhjä, funktio palauttaa tyhjän listan. Jos merkkijono ei ole tyhjä, funktio käyttää `toUpper` funktiota ensimmäisen kirjaimen muuttamiseksi isoksi ja lisää sen merkkijonon alkuun. Lopuksi funktio yhdistää ensimmäisen kirjaimen ja loput merkit `:`-merkillä. 

Tämän lisäksi Haskellissa on myös valmiina funktioita, jotka tekevät saman asian merkkijonon muokkaamiseksi, kuten `capitalize` johon voit tutustua [täältä](http://hackage.haskell.org/package/text/docs/Data-Text.html#v:capitalize). On myös mahdollista kirjoittaa sama funktio käyttämällä esimerkiksi `map` funktiota, joka käy läpi listan ja soveltaa sille annettua funktiota jokaiseen alkioon. Tämä tapahtuu seuraavalla tavalla: `map toUpper "hello"`, joka palauttaa listan `['H', 'E', 'L', 'L', 'O']`.

## Katso myös

- [Haskell Documentation - Data.Char](https://www.haskell.org/onlinereport/standard-prelude.html#char)
- [Haskell Documentation - Text](http://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Haskell Wiki - String](https://wiki.haskell.org/String)