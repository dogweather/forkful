---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mika & Miksi?

Alimerkkijonojen poiminta on prosessi, jossa valitaan osa merkkijonosta sen sijainnin perusteella. Ohjelmoijat tarvitsevat tätä toimintoa esimerkiksi silloin, kun he haluavat analysoida tai muokata olemassa olevaa tekstiä.

## Kuinka:

Alla on esimerkkikoodi, joka näyttää, kuinka erottaa alimerkkijonot Elm-ohjelmoinnissa:

```Elm
import String

main = 
  let
    teksti = "Tervetuloa Elm -ohjelmointiin"
    alku = 0
    loppu = 10
  in
    String.slice alku loppu teksti
```
Tämä koodi palauttaa alimerkkijonon "Tervetuloa ", mikä on merkkijonon alusta alkaen kymmenes merkki.

## Syvällinen tarkastelu:

Historiallinen tausta: Alimerkkijonojen erottelu on peruskäsite monissa ohjelmointikielissä. Elm-ohjelmointikieli on suunniteltu korkealaatuiseksi, ja se on hyvä merkkijonojen käsittelyjärjestelmä.

Vaihtoehtoja: Elm tarjoaa myös muita merkkijonojen käsittelyyn liittyviä toimintoja, kuten String.left ja String.right, jotka palauttavat merkkijonon vasemman tai oikean osan määritellyn merkkimäärän mukaan.

Yksityiskohtia toteutuksesta: String.slice -funktio Elm:ssä hyödyntää JavaScript-toteutusta, ja nojaa vahvasti JavaScriptin sisäisten toimintojen tehokkuuteen.

## Katso Lisää:

1. Elm ohjeet: http://guide.elm-lang.org/
2. Alkuperäinen Elm dokumentaatio: https://elm-lang.org/docs
3. Elm String API: https://package.elm-lang.org/packages/elm/core/latest/String
4. JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice