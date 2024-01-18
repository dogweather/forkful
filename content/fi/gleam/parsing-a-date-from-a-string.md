---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Gleam: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärän erottamista ja muuntamista merkkijonosta objektiksi. Tämä on yleinen etenemistapa kun halutaan muuttaa käyttäjän syöttämä päivämäärä merkkijonona ohjelman ymmärtämään muotoon.

## Miten:
Päivämäärän jäsentäminen merkkijonosta Gleam-ohjelmointikielellä on helppoa. Esimerkiksi, jos haluamme jäsennellä merkkijonon "10/15/2021" päiväksi "15. lokakuuta 2021", voimme käyttää seuraavaa koodia:

```Gleam
Date.fromString("10/15/2021", "%m/%d/%Y")
```

Esimerkissä käytämme `Date.fromString` -funktiota, joka muuntaa merkkijonon päiväksi käyttäen annettua muotoa. Tämän jälkeen voimme tulostaa päivämäärän:

```Gleam
date |> Date.toString("%d. %B %Y") // Tulostaa "15. lokakuuta 2021"
```

## Syväsukellus:
Päivämäärän jäsentämistä merkkijonosta on käytetty jo vuosikymmenten ajan ohjelmoinnissa, ja se on edelleen tärkeä osa monien sovellusten toimintaa. On myös muita tapoja muuntaa päivämäärä merkkijonosta, kuten käyttäen erilaisia säännöllisiä lausekkeita (regex) tai hyödyntämällä valmista kirjastoa.

Gleam-ohjelmointikielen ansiosta päivämäärän jäsentäminen merkkijonosta on helppoa ja turvallista. Ohjelmakoodin tarkistaja auttaa välttämään yleisiä virheitä, kuten väärän muodon antamista päivämäärälle.

Jos haluat lisätietoa Gleam-ohjelmointikielestä ja sen ominaisuuksista, voit tutustua dokumentaatioon osoitteessa [https://gleam.run](https://gleam.run).

## Katso myös:
- [Gleam dokumentaatio](https://gleam.run)
- [Date.fromString dokumentaatio](https://gleam.run/modules/date#from_string)
- [Päivämäärän muotoilu Gleam-ohjelmointikielessä](https://gleam.run/modules/date#to_string)