---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Yhdistämme merkkijonoja, eli teemme merkkijonojen ketjutusta, kun liitämme useita merkkijonoja yhdeksi. Tämä on kätevä tapa luoda dynaamista sisältöä koodauksessa.

## Näin teet:

Fish Shellissa merkkijonon ketjutus on yksinkertaista. Voit yksinkertaisesti luetella merkkijonot peräkkäin ja Fish yhdistää ne. Katsotaan esimerkkiä:

```fish
set tervehdys "Hei"
set nimi "Matti"
echo $tervehdys $nimi
# Tulostuu: Hei Matti
```
Fish Shell ei välitä erityisesti merkkijonon yhdistämisestä (`concatenation`). Se vain liittää merkkijonot yhteen.

## Syvällisemmin:

Merkkijonojen yhdistäminen on ollut ohjelmointikielissä jo pitkään, ja Fish Shell on tehnyt siitä erittäin helppoa. 

Vaihtoehtoina voisi käyttää monia eri ohjelmointikieliä, mutta Fish Shelliä pidetään yksinkertaisena ja selkeänä ratkaisuna.

Merkkijonojen yhdistämisen toteutus Fish Shellissä on yksinkertainen - se vain liittää merkkijonot yhteen, ilman mitään erityistä logiikkaa.

## Katso myös:

Fish Shell Documentation: (https://fishshell.com/docs/current/index.html) Tämä on erinomainen resurssi Fish Shellin oppimiseen, mukaan lukien merkkijonon ketjutus.

Learn The Hard Way: Fish Tutorial (https://learnxinyminutes.com/docs/fish/): Tämä Fish Shell-opas sisältää monia esimerkkejä ja harjoituksia, jotka auttavat oppimaan eri näkökohtia Fish Shellistä, mukaan lukien merkkijonojen yhdistämisen.