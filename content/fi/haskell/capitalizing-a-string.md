---
title:    "Haskell: Merkkijonon kirjoitustyylin muuttaminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Miksi koodataan: Tekstin kirjoituksessa on tärkeää, että otsikot ja muut tärkeät sanat ovat isolla alkukirjaimella. Tämä voidaan hoitaa helposti Haskellilla.


## Kuinka tehdä: Käytämme `Data.Char` moduulia ja sen `toUpper` funktiota muuttaaksemme tekstin ensimmäisen kirjaimen isoksi. Alla on esimerkki:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = toUpper (head str) : tail str

main = do
    let text = "tämä on esimerkki"
    putStrLn $ capitalize text
```
Tulostus: `Tämä on esimerkki`

## Syvällinen perehtyminen: `toUpper` funktio palauttaa ison kirjaimen ASCII koodin avulla. Tämä koodi on yleensä erilainen kullakin tietokoneella, mikä tarkoittaa, että se ei välttämättä tuota haluttua tulosta kaikilla koneilla. Tässä tapauksessa voimme käyttää `unicode-transforms` pakettia ja sen `toUpper` funktiota, joka käsittelee Unicode merkkejä ja takaa halutun tuloksen riippumatta käytetystä koneesta.

# Katso myös:

- [Haskellin virallinen dokumentaatio Data.Char moduulista (englanniksi)](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)
- [Unicode-muunnokset paketti (englanniksi)](http://hackage.haskell.org/package/unicode-transforms)