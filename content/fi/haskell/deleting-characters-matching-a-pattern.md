---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Hahmojen poistaminen käyttäen mallia tarkoittaa tiettyjen merkkijonojen poistamista tekstistä. Ohjelmoijat tekevät tämän esimerkiksi koodin seuraavuuden parantamiseksi tai tiedon esittämiseksi sopivammassa muodossa.

## Miten:

Seuraava koodiesimerkki Haskellissa esittelee, miten voit poistaa kaikki pienet kirjaimet aakkosilta käyttämällä `Data.Char` modulea:

```Haskell
import Data.Char (isLower)
removeLowers :: String -> String
removeLowers = filter (not . isLower)
```

Testikäyttö:

```Haskell
removeLowers "Hei, olen Haskell-ohjelmoija!"
-- Tuottaa: "H, -!"
```

## Syvempi sukellus:

Mallia vastaavien merkkien poistaminen on ollut ohjelmointikielissä useita vuosia. Esimerkiksi Perlissä ja Pythonissa on laajat menetelmät merkkijonojen käsittelyyn.

Haskellissa saatat haluta kirjoittaa oman suodattimesi, mutta useimmiten `Data.Char` tai `Data.Text` moduulit ovat tarpeeksi tehokkaita moniin käyttötarkoituksiin. Huomaa myös, että suodatusfunktio saa parametrikseen ainoastaan yhden merkin kerrallaan.

Haskellissa, toisin kuin joissakin muissa kielissä, ei ole olemassa suoraa tapaa poistaa merkkijonoja toisista merkkijonoista. Sen sijaan meidän pitää suodattaa niitä merkki kerrallaan. Tämä johtuu Haskellin puhtaasta, funktionaalisesta luonteesta.

## Lisätietoa:

Haskellin virallinen dokumentaatio `Data.Char` moduulille: (https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html)

Haskellin virallinen dokumentaatio `Data.Text` moduulille: (https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)

Merkkijonojen manipuloinnin opastus Haskellissa: (http://learnyouahaskell.com/starting-out#an-intro-to-lists)