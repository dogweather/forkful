---
title:                "Merkkijonon pääkirjaintaminen"
html_title:           "Haskell: Merkkijonon pääkirjaintaminen"
simple_title:         "Merkkijonon pääkirjaintaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Kielen merkkijonojen kirjainten muuttaminen isoiksi on helppo tapa tehdä teksti näkyvämmäksi tai standardoida muoto. Yleisimmin tämä on tehtävä, missä se riskejä pienenee typoista tai erilaisista kirjoitustavoista.

## Miten:

Haskellissa voit pääoman kaikki lähdetekstin merkit käyttämällä sisäänrakennettua funktiota 'toUpper'. Se toimii yksittäisille merkkeille, joten meidän täytyy 'map':ata se kokonaiseen merkkijonoon:

```Haskell
import Data.Char (toUpper)

capitalized :: String -> String
capitalized = map toUpper

main :: IO ()
main = putStrLn $ capitalized "hello, world"
```
Esimerkkitulostus olisi:

```Haskell
"HELLO, WORLD"
```

## Syväsukellus:

Haskellissa 'map' sijoittaa funktion kaikkiin listan elementteihin - tässä tapauksessa, kaikkiin Stringin merkkeihin, koska Haskellissa String on vain merkkien luettelo.

Historiallisesti tämä toiminto on peräisin vanhemmista koodauskielistä, joissa merkkijonot koostuivat ASCII-merkeistä. 'A':n ja 'a':n ASCII-arvojen vaihtelevat pääoman mukaan, joten tekstin kirjoittaminen isoksi oli yksinkertainen laskutoimitustapa.

Vaihtoehtoja takomiselle on Haskellissa useita, mukaan lukien 'mapM_' ja 'forM_', mutta 'map' on yleisin ja yksinkertaisin.

## Katso myös:

1. Learn You a Haskell For Great Good: http://learnyouahaskell.com/
2. Real World Haskell: https://book.realworldhaskell.org/
3. Haskell Wiki: https://wiki.haskell.org/
4. Stackage for available Haskell packages: https://www.stackage.org/