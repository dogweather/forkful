---
title:                "Mallin mukaisia merkkejä vastaavien merkkien poistaminen"
html_title:           "Javascript: Mallin mukaisia merkkejä vastaavien merkkien poistaminen"
simple_title:         "Mallin mukaisia merkkejä vastaavien merkkien poistaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Jokaiselle ohjelmoijalle saattaa joskus tulla tarve poistaa tietyt merkit tietyistä kohdista merkkijonossa. Tämä voi helpottaa tietojen muokkausta ja järjestelyä. Tässä artikkelissa opit, miten voit käyttää Javascriptia poistaaksesi merkkejä, jotka täyttävät tietyn mallin.

## Miten

Koodiesimerkki:

```Javascript
// Luo merkkijono, josta poistetaan merkkejä
let merkkijono = "Tämä on esimerkkiteksti!";

// Luo uusi merkkijono, josta poistetaan merkit, jotka täyttävät halutun mallin (esim. välilyönnit)
let uusiMerkkijono = merkkijono.replace(/ /g, "");

// Tulostaa uuden merkkijonon konsoliin
console.log(uusiMerkkijono); //Tämäonesimerkkiteksti!
```

Kuten esimerkistä näkee, voit käyttää `replace()`-funktiota Javascriptilla poistamaan merkkejä tietystä merkkijonosta. Ensimmäisenä parametrina annetaan merkki tai merkkijono, joka halutaan poistaa, ja toisena parametrina haluttu korvaava merkki tai tyhjä merkkijono, mikäli halutaan vain poistaa merkkejä.

## Syvällisempi katsaus

`replace()`-funktio on osa String-objektin ominaisuuksia Javascriptissa. Sillä voidaan korvata merkkejä merkkijonosta, mutta se pystyy myös poistamaan merkkejä, jos toinen parametri annetaan tyhjä merkkijono. Nämä korvasäännöt voi määrittää myös merkkijonon alussa käyttämällä `RegExp`-objektia. Tässä esimerkissä merkkeinä `replace()`-funktiolle annetaan välilyönnit, mutta voit muuttaa mallia haluamallasi tavalla.

## Katso myös

- JavaScript String.replace() - Mozilla Developer Network, https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace