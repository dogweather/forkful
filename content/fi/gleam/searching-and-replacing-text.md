---
title:    "Gleam: Tekstin etsiminen ja korvaaminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi etsiä ja korvata tekstiä Gleam-ohjelmointikielellä? Vastaus on yksinkertainen: tekstin etsiminen ja korvaaminen on olennainen osa ohjelmointia. Se auttaa tekstin manipuloinnissa ja tiedonmuokkauksessa, mikä on tärkeä osa monia ohjelmointiprojekteja.

## Kuinka

Seuraavassa esittelemme kaksi tapaa etsiä ja korvata tekstiä Gleam-ohjelmointikielellä.

```Gleam
// Etsi ja korvaa yksittäinen tekstipätkä

let teksti = "Tervetuloa, maailma!"
let uusi_teksti = teksti |> String.replace("maailma", "Gleam")

// uusi_teksti: "Tervetuloa, Gleam!"
```

```Gleam
// Etsi ja korvaa kaikki esiintymät

let teksti = "Tämä on teksti, jossa on monta sanaa, ja ne kaikki alkavat kirjaimella t."
let uusi_teksti = teksti |> String.replace_all("tä", "Tä")

// uusi_teksti: "Tämä on Teksti, jossa on Monta Sanaa, ja ne kaikki Alkavat kirjaimella t."
```

Tässä esimerkeissä käytämme Gleam-kielen String-moduulia, joka tarjoaa useita toimintoja tekstin muokkaamiseen. Voit myös käyttää säännöllisiä lausekkeita (regular expressions) tekstin hakemiseen ja korvaamiseen Gleamilla.

## Syvemmälle

Etsiminen ja korvaaminen tekstin avulla voi tuntua yksinkertaiselta, mutta Gleam-kieli mahdollistaa monimutkaisempien tekstityökalujen luomisen. Voit esimerkiksi luoda ohjelman, joka etsii tiettyjä sanoja tai lausekkeita ja korvaa ne eri tekstillä riippuen kontekstista. Voit myös yhdistää tekstin etsimis- ja korvaustoiminnot muihin Gleam-kielen toimintoihin, kuten tiedostonkäsittelyyn ja tulostamiseen.

## Katso myös

- [String-moduulin dokumentaatio Gleam-kielen virallisessa verkkosivustossa](https://gleam.run/modules/string/)
- [Gleam-ohjelmointikurssi (eng)](https://dev.to/gleam_language/beginner-friendly-gleam-tutorial-series-1ho8)
- [Gleam-kielen GitHub-repositorio](https://github.com/gleam-lang/gleam)