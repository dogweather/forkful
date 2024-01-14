---
title:    "Elm: Isojen kirjainten muuttaminen pieniksi ohjelmoinnissa"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi: 

Miksi kukaan haluaisi muuttaa merkkijonon pienaakkosiin? 

## Miten:

Muotoillut esimerkit koodista ja tulosteista "```Elm ... ```" koodilohkojen sisällä: 

```Elm
// Luodaan merkkijono, joka sisältää sekä isoja että pieniä kirjaimia
let merkkijono = "EKM Provigil"

// Käytetään Elm:n String -moduulin toimintoa muuttaaksemme kaikki kirjaimet pieniksi
let pienetKirjaimet = String.toLower merkkijono

// Tulostetaan muutettu merkkijono konsolille
konsoliin pienetKirjaimet  // tulostaa "ekm provigil" 
```

## Syvällinen sukellus:

Merkkijonon muuttaminen pienaakkosiin voi olla hyödyllistä monissa eri tilanteissa, kuten tietokannassa tehtävien hakuessa, lomakkeiden validoinnissa ja monissa muissa. Pienaakkoset tarjoavat myös yhtenäisen ja helppolukuisen merkistyksen, mikä tekee koodista helpommin ymmärrettävää. Elm:n String -moduulin toiminnot auttavat helposti muuttamaan merkkijonot pienaakkosiin tai suuraakkosiin tarvittaessa. 

## Katso myös:

- Elm String -dokumentaatio (https://package.elm-lang.org/packages/elm-lang/core/latest/String)
- Pienaakkosmuunnoksen selitys Stack Overflow:ssa (https://stackoverflow.com/questions/513832/how-to-convert-string-to-lowercase-in-javascript)
- Artikkeli merkkijonojen käsittelystä Elm-kielellä (https://medium.com/elm-shorts/handling-strings-in-elm-2383b348be7d)