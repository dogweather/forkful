---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Gleam: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan halunnut muuttaa tiettyjä tekstejä koodistasi? Gleamilla tämä onnistuu helposti ja nopeasti.

## Kuinka tehdä

Tarvitset vain `replace` toiminnon ja sen parametreiksi annetaan etsittävä teksti, korvaava teksti ja alkuperäinen teksti, jota haluat muokata. Katso alla Gleamilla kirjoitettu esimerkki:

```Gleam
replace("vanha", "uusi", "Tämä on vanha teksti.")
```

Tämä koodi korvaa `Tämä on vanha teksti` tekstin `Tämä on uusi teksti` tekstiksi. Helppoa, eikö?

## Syvemmälle

Jos haluat tehdä monimutkaisempia muutoksia tekstissä, voit käyttää `regex_replace` funktiota. Se mahdollistaa säännöllisten lausekkeiden käytön, jolloin voit etsiä ja korvata tekstejä tietyillä säännöillä.

Tässä esimerkki, jossa haluamme vaihtaa kaikki numerot tekstin "numero" tekstiksi:

```Gleam
regex_replace("\\d", "numero", "123 - tämä sisältää numeroita.")
```

Tämä tuottaa seuraavan tuloksen: `numero - tämä sisältää numeroita.`

## Katso myös

* Gleamin dokumentaatio "String" moduulista: https://gleam.run/modules/std/string/
* Regular expressions oppimateriaalit: https://regexone.com/ ja https://www.regular-expressions.info/