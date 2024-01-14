---
title:    "Haskell: Pilkkoen alimerkkijonoja"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Miksi kannattaa tutkia alamerkkijonojen erottelua Haskellilla?

Kaikissa ohjelmoinnin kielissä on tärkeää olla tehokas ja suorituskykyinen, ja tämä pätee myös Haskellissa. Yksi tapa parantaa koodin tehokkuutta on alamerkkijonojen erottelu, joka voi helpottaa esimerkiksi merkkijonojen käsittelyä ja analysointia. Seuraavassa osiossa kerron tarkemmin, miten alamerkkijonojen erottelu tapahtuu Haskellilla.

## Kuinka tehdä

Alamerkkijonojen erottelu Haskellilla on hyvin helppoa! Käytämme funktiota "take" ja "drop" yhdessä "pack" ja "unpack" funktioiden kanssa.

```Haskell
> take 5 (unpack "tervetuloa")
"terve"
```

Yllä olevassa esimerkissä otamme ensimmäiset viisi merkkiä merkkijonosta "tervetuloa" käyttämällä "take" funktiota. Samaan aikaan käytämme "unpack" funktiota muuttaaksemme merkkijonon listaksi merkeistä. Näin voimme käsitellä merkkijonoja helposti listoina.

Käänteinen prosessi, eli alamerkkijonon poistaminen, tapahtuu käyttämällä "drop" funktiota.

```Haskell
> drop 5 (unpack "tervetuloa")
"tuloa"
```

Kaiken kaikkiaan alamerkkijonojen erottelu on yksinkertaista ja tehokasta Haskellilla!

## Syväsukellus

Voi olla hyödyllistä ymmärtää tarkemmin, miten "take" ja "drop" funktiot toimivat alamerkkijonojen erottelussa. "take" funktio ottaa ensimmäiset "n" elementtiä listalta ja palauttaa uuden listan niistä. "drop" funktio taas poistaa ensimmäiset "n" elementtiä listalta ja palauttaa uuden listan niiden jälkeen.

"pack" funktio puolestaan muuttaa merkkijonon listaksi merkeistä, kun taas "unpack" tekee päinvastaisen. Tämä on hyödyllistä, sillä listoja on helpompi käsitellä ja manipuloida kuin yksittäisiä merkkejä.

Voit lukea lisää näistä ja muista merkkijonoihin liittyvistä funktioista [Haskellin dokumentaatiosta](https://hackage.haskell.org/package/base/docs/Data-List.html#v:take) ja [Learn You a Haskellasta](http://learnyouahaskell.com/starting-out#texas-ranges).

## Katso myös

- [Haskellin dokumentaatio merkkijonoista](https://hackage.haskell.org/package/base/docs/Data-String.html)
- ["Take" ja "drop" funktioiden selitykset](https://hackage.haskell.org/package/base/docs/Data-List.html#v:take)