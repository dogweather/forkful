---
title:                "Merkkijonon interpolointi"
html_title:           "Javascript: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

Interpolointi JavaScriptissä - Mitä se on ja miksi se on tarpeellista

## Mitä & Miksi?
Interpolointi tarkoittaa merkkijonon sisällön yhdistämistä muuttujiin tai tietoihin. Tämä mahdollistaa dynaamisen sisällön luomisen ja muokkaamisen ohjelmointikoodissa. Interpolointi tekee koodista selkeämpää ja helpottaa muuttujien käyttöä merkkijonojen sisällä.

## Näin teet sen:
Interpoloinnin suorittamiseksi JavaScriptissä käytetään merkkijonon sisällä backtick-merkkejä (``) sekä dollari-merkkiä ($). Tämän avulla voit yhdistää muuttujan sisällön suoraan merkkijonoon. Esimerkiksi: 
```
const nimi = 'Matti';
console.log(`Hei ${nimi}!`); // Tämä tulostaa: Hei Matti!
```

## Syvemmälle sukeltaminen:
Interpolointi alun perin esiteltiin JavaScriptiin vuonna 2015 ECMAScript 6 -versiossa. Sitä ennen muuttujien yhdistäminen merkkijonoihin tapahtui käyttämällä plus-merkkiä (+). Interpolointi mahdollistaa myös monipuolisemman koodin luomisen lyhyemmällä ja selkeämmällä syntaksilla.

## Katso myös:
Voit lukea lisää interpoloinnista ja sen käytöstä esimerkiksi MDN:n sivuilta: https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/template_strings.