---
title:    "Haskell: Tekstitiedoston kirjoittaminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Tekstin kirjoittaminen on tärkeä osa Haskell-ohjelmointia. Se mahdollistaa tiedon tallentamisen ja jakamisen helposti ohjelman käytön aikana.

## Miten

Tekstin kirjoittaminen on yksinkertaista Haskellissa. Alla on esimerkki, miten luodaan tekstiä ja tallennetaan se tiedostoon:

```Haskell
kirjoitaTeksti :: String -> IO ()
kirjoitaTeksti teksti = writeFile "tekstifile.txt" teksti
```

Koodin selitys:

- `kirjoitaTeksti` on funktio, joka ottaa vastaan `String`-muuttujan ja palauttaa `IO`-toiminnon.
- `writeFile` on valmiiksi olemassa oleva funktio, joka ottaa vastaan tiedoston nimen ja tekstiä ja tallentaa tekstin annettuun tiedostoon.
- `"tekstifile.txt"` on tiedoston nimi, johon teksti tallennetaan.
- `teksti` on annettu teksti, joka tallennetaan tiedostoon.

Kun funktio on määritelty, se voidaan suorittaa komennolla `kirjoitaTeksti "Hei, maailma!"`. Tämän jälkeen tiedostoon `tekstifile.txt` tallentuu teksti "Hei, maailma!".

## Syvä sukellus

Tekstin kirjoittaminen on yksi monista IO-toiminnoista Haskellissa. IO-toiminnot ovat hieman erityyppisiä verrattuna muihin Haskellin funktioihin, sillä ne voivat vaikuttaa ohjelman tilaan ja ympäristöön. Tästä syystä IO-toiminnot tulee suorittaa `main`-funktion sisällä. Eli jos haluat tallentaa tekstin tiedostoon, se tulee tehdä `main`-funktion sisällä.

Lisäksi on hyvä tiedostaa tiedoston olemassaolo osoitteessa, johon teksti tallennetaan. Jos tiedostoa ei ole olemassa, `writeFile` luo sen automaattisesti.

## Katso myös

- [Haskell-kielto](https://fi.wikipedia.org/wiki/Haskell)
- [Haskell-tutoriaali](https://learnxinyminutes.com/docs/fi-fi/haskell-fi/)
- [Haskell IO](https://www.haskell.org/tutorial/io.html)