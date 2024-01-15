---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Javascript: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Joskus haluat tarkistaa, onko tietyssä kansiossa olemassa olevia tiedostoja ennen kuin suoritat tiettyjä toimintoja. Tämä voi auttaa välttämään virheitä ja parantamaan koodisi suorituskykyä.

## Kuinka tehdä

Tarkistaaksesi, onko kansio olemassa, voit käyttää `fs.existsSync()`-funktiota. Tämä funktio hyväksyy yhden argumentin, joka on polku tarkistettavaan kansioon. Se palauttaa boolean-arvon, joka ilmaisee, onko kansio olemassa vai ei.

```
```Javascript
const fs = require('fs');

if(fs.existsSync('/polku/tarkistettavaan/kansioon')){
    console.log('Kansio löytyi!');
} else {
    console.log('Kansiota ei löytynyt.');
}
```

Näet, että käytämme `if`-lausekkeita tarkistamaan, onko funktion palauttama arvo `true` vai `false`. Voit myös käyttää tätä funktiota `try/catch`-rakenneella käsitelläksesi virheitä, jos kansio ei ole olemassa. Tässä on esimerkki:

```
```Javascript
try {
    fs.existsSync('/polku/tarkistettavaan/kansioon');
    console.log('Kansio löytyi!');
} catch(err) {
    console.log('Kansiota ei löytynyt.');
}
```

Tämä on hyödyllistä, jos haluat estää ohjelmasi kaatumisen, jos kansio ei ole olemassa.

## Syvällinen sukellus

`fs.existsSync()`-funktio kuuluu Node.js:n `fs`-moduuliin, joka tarjoaa rajapinnan tiedostojärjestelmään. Tämä moduuli sisältää myös muita hyödyllisiä toimintoja tiedostojen ja kansioiden hallintaan. Lisää tietoa löydät [Node.js:n virallisesta dokumentaatiosta](https://nodejs.org/api/fs.html).

## Katso myös

- [Node.js:n `fs`-moduulin dokumentaatio](https://nodejs.org/api/fs.html)
- [Tiedostojärjestelmän hallinta Node.js:ssä](https://www.digitalocean.com/community/tutorials/how-to-use-node-js-to-manage-file-system)
- [Tehtävä: Tarkista, onko kansio olemassa](https://www.freecodecamp.org/news/node-js-check-if-a-file-is-existing-or-not-fa47024023ef/)