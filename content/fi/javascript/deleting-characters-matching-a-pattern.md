---
title:    "Javascript: Kuvion mukaisten merkkien poistaminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi
Monissa tilanteissa Javascript-ohjelmoijat joutuvat kohtaamaan tilanteen, jossa heidän täytyy poistaa merkkejä, jotka täyttävät tietyn kaavan. Tämä voi olla tarpeellista esimerkiksi käyttäjän tekstisyötön tarkistamisessa tai datan käsittelyssä.

## Miten
Javascriptillä on erilaisia ​​metodeja ja funktioita, jotka mahdollistavat merkkien poistamisen kaavan perusteella. Yksi yleisimmin käytetyistä on `replace()`-funktio, joka ottaa vastaan kaksi parametria: poistettavan kaavan ja sen tilalle tulevan merkkijonon. Esimerkiksi seuraavalla koodilla voimme poistaa kaikki pisteet ja pilkut merkkijonosta:

```Javascript
let teksti = "Tänään olemme juhlineet Suomen itsenäisyyttä.";
let uusiTeksti = teksti.replace(/[,\.]/g, ""); // palauttaa "Tänään olemme juhlineet Suomen itsenäisyyttä"

console.log(uusiTeksti);
```

Koodissa käytetään ns. regular expression -kaavaa, joka merkitsee pilkkua tai pistettä (`[,\.]`) ja `g`-lipuketta, joka merkitsee, että kaavaa sovelletaan kaikkiin merkkeihin merkkijonossa. Tämän ansiosta kaikki pisteet ja pilkut poistetaan ja lopputulokseksi saadaan alkuperäinen teksti ilman niitä.

## Syvällisempi tarkastelu
`replace()`-funktion lisäksi Javascriptillä on muitakin käteviä metodeja, joilla voi poistaa merkkejä kaavan perusteella. `match()`-funktio esimerkiksi etsii annetusta merkkijonosta kaikki kaavan täyttävät merkit ja palauttaa ne taulukkona. `split()`-funktio puolestaan pilkkoo merkkijonon kaavan täyttävien merkkien kohdalta ja palauttaa siitä taulukon.

On myös mahdollista käyttää erilaisia ​​kaavojen yhdistelmiä ja käyttää hyväkseen erilaisia metodeja yhtäaikaisesti löytääkseen ja poistaakseen tietystä merkkijonosta kaikki halutut merkit.

## Katso myös
- [MDN Web Docs: replace()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/replace) (englanniksi)
- [MDN Web Docs: match()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/match) (englanniksi)
- [MDN Web Docs: split()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/split) (englanniksi)
- [RegExr](https://regexr.com/) - verkkosivu, jolla voi testata ja luoda regular expression -kaavoja