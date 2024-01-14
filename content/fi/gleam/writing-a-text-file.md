---
title:    "Gleam: Tekstitiedoston kirjoittaminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluat kirjoittaa tekstitiedoston Gleam-ohjelmointikielellä? Yksi syy voi olla tarve tallentaa tietoa tai lukea tietoja tiedostosta, jolloin tekstitiedosto on kätevä tapa hallita tietoa.

## Miten

Gleam tarjoaa kätevän tavan luoda ja kirjoittaa tekstitiedostoja. Se voidaan tehdä käyttämällä `Text.open`-funktiota, joka avaa tiedoston annetusta polusta. Tämän jälkeen voidaan käyttää `Text.write`-funktiota kirjoittamaan haluttu sisältö tiedostoon. Alla on esimerkki koodista ja sen tuottamasta tulosteesta.

```Gleam
let tiedosto = Text.open("tiedosto.txt")
Text.write(tiedosto, "Tämä on tekstisisältö, joka kirjoitetaan tiedostoon.")
```

Tulosteen pitäisi näyttää tältä:

```
Tämä on tekstisisältö, joka kirjoitetaan tiedostoon.
```

## Syväsukellus

Tiedostojen kirjoittaminen Gleamilla tarjoaa monia mahdollisuuksia. `Text.write`-funktion lisäksi Gleam tarjoaa myös muita hyödyllisiä toimintoja, kuten `Text.write_line`, joka kirjoittaa rivin kerrallaan, ja `Text.format`, joka mahdollistaa muuttujan arvojen lisäämisen tekstisisältöön. Näiden toimintojen lisäksi Gleamilla on myös mahdollista käyttää `File`-moduulia, joka tarjoaa laajemman valikoiman mahdollisuuksia tiedostojen käsittelyyn.

## Katso myös

- Gleam-ohjelmointikielen virallinen dokumentaatio (englanniksi): https://gleam.run/documentation/
- Gleam-ohjelmointikielen kotisivu (englanniksi): https://gleam.run/