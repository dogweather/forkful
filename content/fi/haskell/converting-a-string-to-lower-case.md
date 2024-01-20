---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lauseen muuntaminen pienaakkosiksi tarkoittaa sitä, että kaikki lauseen isot kirjaimet muutetaan pieniksi kirjaimiksi. Ohjelmoijat tekevät tämän usein helpottaakseen tekstin käsittelyä ja vertailua, koska koneet pitävät erikokoisia kirjaimia eri merkkeinä.

## Miten näin tehdään:

Haskellin standardikirjastossa on `Data.Char` moduuli, joka tarjoaa toLower-funktion. Se muuntaa merkin pieniksi kirjaimiksi. Tässä on esimerkki sen käytöstä:

```Haskell
import Data.Char (toLower)

lowerCaseString :: String -> String
lowerCaseString str = map toLower str

main :: IO ()
main = putStrLn $ lowerCaseString "Hei SUOMI!"
```

Tämä ohjelma tulostaa `"hei suomi!"`.

## Syvempi sukellus:

Näyttää siltä, että yksinkertainen toiminto, kuten merkkijonon muuttaminen pieniksi kirjaimiksi, ei vaadi paljon historiallista kontekstia tai syvempää ymmärrystä. Kuitenkin, kuten monissa ohjelmointikysymyksissä, se liittyy suoraan tietokoneiden ja ohjelmointikielten ongelmiin tekstin, numeroiden ja binääridatan käsittelyssä.

- Historiallinen konteksti: `toLower`-funktion kaltaisten toimintojen tarve johtuu siitä, että eri kirjaimet (iso ja pieni) on koodattu eri numeroiksi ASCII-taulukossa, jota tietokoneet ja ohjelmointikielet käyttävät merkkijonojen käsittelyyn.

- Vaihtoehdot: Voit ensin tarkistaa, onko merkki iso kirjain, ja sitten muuttaa sen pieneksi kirjaimiksi. Tämä ei kuitenkaan ole tehokasta, kun voit suoraan käyttää `toLower`-muunnosta.

- Toteutuksen yksityiskohdat: `toLower`-muunnos toteutetaan tavallisesti yksinkertaisena numeroiden siirtona. ASCII-kooditaulukossa pienet kirjaimet ovat samat kuin isot kirjaimet, mutta ne on siirretty 32 yksikköä. Joten `toLower`-muunnos vain lisää 32 numeroon, joka vastaa isoa kirjainta.

## Katso myös:

Lisätietoja Haskellin merkkijonojen käsittelystä ja `Data.Char`-moduulista löydät seuraavista lähteistä:

- Haskell Library: [Data.Char](http://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)