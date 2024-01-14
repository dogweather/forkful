---
title:    "Elm: Kahden päivämäärän vertailu"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnin yhteydessä joudutaan vertailemaan kahta päivämäärää. Tämä voi olla tarpeen esimerkiksi tietyn tapahtuman järjestämisessä tai tietyn ajanjakson tarkastelemisessa. Elm-kielellä tämä onnistuu helposti ja tehokkaasti.

## Kuinka tehdä

Vertailun toteuttamiseksi tarvitaan kaksi erillistä päivämäärää, jotka annetaan Date-moduulin Date-tyyppinä. Esimerkiksi:

```Elm
date1 = Date.fromIsoString "2021-09-01"
date2 = Date.fromIsoString "2021-10-01"
```

Päivämäärät voidaan nyt vertailla käyttämällä Elm-kieleen sisäänrakennettua Date.compare-funktiota. Tämä funktio palauttaa DateValue-tyypin arvon -1, 0 tai 1 riippuen siitä, kumpi päivämäärä on aiempi. Esimerkiksi:

```Elm
Date.compare date1 date2 -- Tämä palauttaa arvon -1, koska date1 on aiempi kuin date2
Date.compare date2 date1 -- Tämä palauttaa arvon 1, koska date2 on myöhempi kuin date1
```

## Syvällisempi sukellus

Date.compare-funktion lisäksi Elm-kielestä löytyy muitakin hyödyllisiä toimintoja, joita voi käyttää päivämäärien vertailussa. Esimerkiksi Date.between-funktio voi tarkistaa, onko tietyssä ajankohtana annettu päivämäärä kahden muun päivämäärän välissä. Date.diff-funktio taas laskee päivämäärien välisen ajanjakson päivinä, kuukausina ja vuosina.

## Katso myös

- [Elm Date -dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/Time-Date/)
- [Elm Date-moduulin esimerkkejä](https://guide.elm-lang.org/interop/dates.html)
  Kurkkaa myös muut jännittävät Elm-kieleen liittyvät artikkelit ja resurssit osoitteessa [elm-lang.org](https://elm-lang.org/).