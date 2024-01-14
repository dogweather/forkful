---
title:    "Elm: Tiedoston kirjoittaminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Käytännöllisesti katsoen jokaisessa ohjelmointiprojektissa tarvitaan tarvitaan jonkinlaisen tekstitiedoston kirjoittamista. Se voi olla esimerkiksi konfiguraatiotiedostoa, lokitiedostoa tai käyttäjän syöttämää tietoa tallentavaa tiedostoa. Tekstitiedostot ovat olennainen osa ohjelmointia, joten on tärkeää tietää kuinka niitä kirjoitetaan.

## Miten

Tekstitiedoston kirjoittaminen on helppoa Elm-ohjelmointikielessä. Se voidaan tehdä käyttämällä `text`-moduulia ja sen `writeFile`-funktiota. Katso esimerkki alla:

```elm
import Text exposing (writeFile)

kirjoitaTiedosto : String -> Cmd msg
kirjoitaTiedosto sisältö =
    let
        polku = "tiedosto.txt"
    in
    writeFile polku sisältö
```

Funktion `kirjoitaTiedosto` parametri on tiedoston sisältö merkkijonona ja funktion tulosteena saadaan komento `Cmd msg`, joka suorittaa tiedoston kirjoittamisen. Huomaa myös, että meidän täytyy antaa tiedostolle polku, jossa se tallennetaan. Tässä tapauksessa tiedosto tallennetaan tiedostonimeen "tiedosto.txt".

## Syvemmälle

Elm tarjoaa myös muita vaihtoehtoja tekstitiedoston kirjoittamiseen, kuten `Text.Write`-moduulin, joka mahdollistaa tiedoston kirjoittamisen ilman erillisiä polkuja ja komentoja. Voit myös käyttää `Text.toFile`-funktiota, joka vastaanottaa tiedostonimen ja tiedoston muodostavan funktion parametreina.

## Katso myös

- [Elm Text -dokumentaatio](https://package.elm-lang.org/packages/elm-lang/core/1.0.5/Text)
- [Ohjeet tekstitiedostojen kirjoittamiseen Elmissä](https://elmprogramming.com/writing-text-files-in-elm.html)