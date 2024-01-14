---
title:    "Haskell: Tarkistetaan, onko hakemisto olemassa"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa on tarve tarkistaa, onko tietty kansio olemassa. Tämä voi liittyä esimerkiksi tarpeeseen luoda tai lukea tiedostoja tietyssä hakemistossa. Tässä blogikirjoituksessa tarkastelemme, miten tämä voidaan tehdä Haskell-ohjelmointikielellä.

## Miten

Haskellissa kansio-olioita käytetään hallitsemaan tiedostojärjestelmää. Olemassa olevan hakemiston tarkistamiseksi käytämme `doesDirectoryExist`-funktiota. Se ottaa parametrinaan kansion polun ja palauttaa `IO Bool`-arvon, joka kertoo, onko kansio olemassa vai ei.

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let path = "kansio"
  exists <- doesDirectoryExist path
  if exists
    then putStrLn "Kansio on olemassa"
    else putStrLn "Kansiota ei ole olemassa"
```

Yllä olevassa esimerkissä luodaan `path`-muuttujaan kansion polku ja sitten `doesDirectoryExist`-funktiolla tarkistetaan, onko kyseinen kansio olemassa. Tämän jälkeen tulostetaan käyttäjälle haluttu viesti sen mukaan, mikä paluuarvo on.

## Syvemmälle

Kansion tarkistamisessa kannattaa olla tarkkana käyttäjän antamien polkujen kanssa. On hyvä idea validoida polku esimerkiksi `System.FilePath`-moduulin avulla ja ottaa huomioon myös käyttöjärjestelmän polkuseparaattori. Lisäksi voi olla hyödyllistä tutustua `System.Directory`-moduulin muihin funktioihin ja mahdollisuuksiin esimerkiksi kansioiden luomiseen tai poistamiseen.

## Katso myös

- [Haskellin System.Directory-dokumentaatio](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskellin System.FilePath-dokumentaatio](https://hackage.haskell.org/package/filepath/docs/System-FilePath.html)