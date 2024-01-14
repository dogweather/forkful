---
title:    "Elm: Alaryhmien erottaminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen tuntuu usein tarpeettomalta tai vaikealta tehtävältä, mutta se voi olla hyödyllistä monissa tilanteissa. Se auttaa esimerkiksi käsittelemään ja muokkaamaan merkkijonoja tarkemmin ja tehokkaammin. Elm tarjoaa yksinkertaisen ja selkeän tavan erottaa substringit, mikä tekee siitä erinomaisen vaihtoehdon tähän tehtävään.

## Näin teet sen

```Elm
-- Otetaan esimerkiksi merkkijono "Tervetuloa Elm-maailmaan"
substring 4 10 "Tervetuloa Elm-maailmaan"

-- Output: "etuloa"
```

Yllä olevassa koodiesimerkissä käytämme `substring` -funktiota erottaaksemme merkkijonon "Tervetuloa Elm-maailmaan" neljännestä indeksistä kuudenteen indeksiin, eli kirjaimet "etuloa". `substring` -funktio ottaa kolme parametria: ensimmäisen indeksin, josta alkaen substring otetaan, viimeisen indeksin, johon asti substring otetaan, sekä merkkijonon, josta substring erotetaan.

## Syvään sukellus

`substring` -funktion lisäksi Elm tarjoaa myös muita hyödyllisiä funktioita substringien hallintaan. Esimerkiksi `slice` -funktio toimii samalla tavalla kuin `substring`, mutta sen avulla voi määrittää indeksit myös negatiivisina lukuina, mikä helpottaa substringien erottamista lopusta alkuun. `indexOf` -funktio puolestaan palauttaa ensimmäisen esiintymän indeksin halutulle merkkijonolle.

On myös hyvä huomata, että `substring` -funktio ottaa parametrikseen indeksit merkkien sijainnille, ei itse merkkien lukumäärää. Tämä tarkoittaa sitä, että tietyillä merkeillä, kuten ääkkösillä, voi olla useampi kuin yksi indeksi.

## Katso myös

- [Substring - Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/String#substring)
- [Slice - Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- [IndexOf - Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/String#indexOf)