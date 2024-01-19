---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Hakeminen ja korvaaminen tarkoittaa tekstijoukossa olevan merkkijonon etsimistä ja sen korvaamista toisella. Se on hyödyllinen ohjelmointitoiminto, koska se auttaa tekemään suuria muutoksia koodiin nopeasti ja vaivattomasti.

## Miten?
Käyttämällä Gleam-ohjelmointikielen `replace`-funktiota, voimme helposti suorittaa hakemisen ja korvaamisen. Katso alla oleva esimerkkikoodi:

```Gleam
import gleam/string

pub fn main() {
  let old = "vanha"
  let new = "uusi"
  let text = "Tämä on vanha teksti."

  let result = string.replace(old, new, text)

  assert result == "Tämä on uusi teksti."
}
```

Kun tämä ohjelma suoritetaan, se tuottaa outputiksi: "Tämä on uusi teksti."

## Syvä Sukellus
1. Historiallinen tausta: Hakeminen ja korvaaminen ovat olleet ohjelmointikielten perustoimintoja vuosikymmeniä. Ne otettiin ensimmäisen kerran laajasti käyttöön tekstieditorissa, joka oli osa alunperin 1970-luvulla kehitettyä Unix-käyttöjärjestelmää.

2. Vaihtoehdot: Useimmissa ohjelmointikielissä hakemisen ja korvaamisen voi toteuttaa useammalla kuin yhdellä tavalla. Esimerkiksi Gleamissa on muitakin tapoja tehdä tämä, kuten käyttämällä hajautettua datarakennetta.

3. Toteutus: Hakemisen ja korvaamisen toteutus vaihtelee kielestä toiseen. Gleamissa `replace`-funktio käy läpi koko tekstin ja korvaa jokaisen löydetyn merkkijonon yksitellen.

## Katso Myös
Gleam-ohjelmiston [dokumentaatio](https://hexdocs.pm/gleam_stdlib/Gleam/String.html#replace/3) tarjoaa kattavan esittelyn siihen, miten voit käyttää `replace` toimintoja ja muita merkkijonojen käsittelyyn tarvittavia toimintoja. Lisäksi, [Gleam-ohjelmoinnin opas](https://gleam.run/book/) sisältää hyödyllisiä vinkkejä ja parhaita käytäntöjä Gleam-ohjelmoinnille.