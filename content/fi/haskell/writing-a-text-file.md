---
title:                "Haskell: Tekstitiedoston kirjoittaminen"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi kirjoittaa tekstitiedostoa Haskellilla? Yksinkertaisesti, koska Haskell on loistava ohjelmointikieli ja mahdollistaa tehokkaan ja tarkan tavan käsitellä tekstiä.

## Miten

Haskell on funktionaalinen ohjelmointikieli, joka perustuu vahvasti monadisiin rakenteisiin. Tämä tekee siitä loistavan valinnan tekstieditorien ja muiden tekstikäsittelyohjelmien kirjoittamiseen.

```
Haskell
import System.IO

main = do
    file <- openFile "tekstitiedosto.txt" WriteMode
    hPutStrLn file "Tämä on tekstiä."
    hPutStrLn file "Tämä on toinen rivi."
    hClose file
```

Tämä yksinkertainen esimerkki avaa tekstitiedoston nimeltä "tekstitiedosto.txt" ja kirjoittaa siihen kaksi riviä tekstiä. Tämän jälkeen tiedosto suljetaan. Voit myös käyttää muita Haskellin IO-funktioita, kuten hGetLine ja hPutStr, saadaksesi myös syötettä ja tulostusta.

## Syvemmälle

Tekstitiedostojen kirjoittaminen Haskellilla voi vaikuttaa aluksi hieman monimutkaiselta, mutta kun opit käyttämään monadeja ja IO-toimintoja oikein, huomaat kuinka tehokasta ja joustavaa se voi olla. Muista myös käsitellä mahdollisia virhetilanteita, kuten tiedostojen avaamisen tai kirjoittamisen epäonnistumista.

## Katso myös

- [Haskellin IO-dokumentaatio](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Monadit ja IO Haskellissa](https://www.haskell.org/tutorial/io.html)
- [Haskelliopas](https://wiki.haskell.org/Haskel