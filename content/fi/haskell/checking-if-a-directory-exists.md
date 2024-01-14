---
title:                "Haskell: Tarkastetaan, onko kansio olemassa"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa hakemisto?

Haskell on voimakas ja monipuolinen ohjelmointikieli, joka tarjoaa erilaisia ​​työkaluja ja toimintoja ohjelmoijille. Yksi näistä toiminnoista on tarkistaa, onko tietty hakemisto olemassa. Tämä voi olla hyödyllistä monissa eri tilanteissa, esimerkiksi kun halutaan varmistaa, että olemassa oleva tiedosto voidaan avata tai kun tarvitaan tiettyjä tiedostoja säännöllisesti suoritettavaan ohjelmaan. Tässä blogikirjoituksessa tutustutaan tarkemmin siihen, miten tarkistaa, onko hakemisto olemassa Haskellissa.

## Miten tehdä se

Ensimmäinen askel on tuoda `System.Directory` kirjasto käyttöön:

```Haskell
import System.Directory
```

Tämän jälkeen voimme käyttää `doesDirectoryExist` funktiota tarkistamaan haluamamme hakemiston olemassaolon:

```Haskell
doesDirectoryExist "/polku/hakemistoon"
```

Tämä palauttaa `True`, jos hakemisto löytyy, ja `False`, jos se ei ole olemassa. Voimme myös käyttää `getCurrentDirectory` funktiota saadaksemme nykyisen hakemiston ja tarkistaa sen olemassaolon:

```Haskell
getCurrentDirectory >>= doesDirectoryExist
```

Tulos olisi myös `True` tai `False` riippuen nykyisen hakemiston olemassaolosta.

## Syventävä tarkastelu

On myös hyvä huomata, että `doesDirectoryExist` funktio heittää poikkeuksen, jos annettua polkua ei voi tarkistaa, esimerkiksi jos annettu polku ei ole hakemisto tai jos polku on virheellinen.

Tarkistettaessa hakemistoa, joka sisältää välilyöntejä, on myös käytettävä `makeAbsolute` funktiota, joka muuttaa annetun polun absoluuttiseksi. Näin välilyönnit eivät aiheuta ongelmia tarkistuksessa.

## Katso myös

- [Hakemiston tarkistaminen Haskellissa - Dokumentaatio](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- [Lyhyt esimerkki hakemiston tarkistamisesta - Stack Overflow](https://stackoverflow.com/questions/40756100/check-if-directory-exists-in-haskell)
- [Haskellin virallinen kotisivu](https://www.haskell.org/)

Kiitos, että luit tämän blogikirjoituksen! Toivottavasti se auttoi sinua oppimaan lisää siitä, miten tarkistaa hakemistoja Haskellissa. Muista käyttää näitä taitoja omassa koodauksessasi ja jatkaa oppimista!