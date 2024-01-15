---
title:                "Työskentely yaml:n kanssa"
html_title:           "Javascript: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet JavaScript-kehittäjä ja haluat tehokkaan ja helpon tavan hallita tietoja, jotka ovat tallennettu tekstiin, niin YAML on juuri sitä mitä tarvitset. Se on moderni tiedostomuoto, joka on helppo lukea ja kirjoittaa, mikä tekee siitä erinomaisen vaihtoehdon monimutkaisille tietoformaateille, kuten XML.

## Miten

YAML-tiedostojen käsittely JavaScriptissä on yksinkertaista ja suoraviivaista. Ensimmäiseksi varmista, että olet asentanut tarvittavan kirjaston, kuten [js-yaml](https://github.com/nodeca/js-yaml), käyttääksesi YAML-parseria.

```Javascript
// Esimerkki YAML-tiedostosta
const yamlData = `
  year: 2021
  month: August
  day: 24
`;
// Muunnetaan YAML JavaScript-objektiksi
const jsObject = YAML.load(yamlData);
// Tulostetaan tiedot konsolille
console.log(`Tänään on ${jsObject.day}. ${jsObject.month} ${jsObject.year}.`);
```

Tämä koodi luo JavaScript-objektin, joka sisältää vuoden, kuukauden ja päivän tiedot YAML-tiedostosta. Huomaa, että YAML syntaksiin kuuluu sisennys merkityillä tasoilla, joten varmista että tiedostosi on oikeassa muodossa ennen kuin yrität muuntaa sen JavaScript-muotoon.

## Syventävä sukellus

YAML-tiedot voivat sisältää monenlaisia arvoja, kuten lukuja, tekstiä, listoja ja objekteja. Voit myös luoda muuttujia ja käyttää niitä myöhemmin tiedostossa. Tässä on toinen esimerkki muokatusta YAML-päivämäärätiedostosta:

```Javascript
date: &date
  year: 2021
  month: August
  day: 24

// Käytetään muuttujina
date1: *date
date2: *date
// Muutetaan kuukausi ja päivä
date2.month: September
date2.day: 1
```

Tämä tuottaa kaksi JavaScript-objektia, joista kummallakin on sama vuosi, mutta eri kuukausi ja päivä. Tämä voi olla hyödyllistä, jos haluat käyttää samoja tietoja useissa eri kohdissa tiedostoa.

## Katso myös

- [js-yaml](https://github.com/nodeca/js-yaml)
- [YAML-oppaat](https://yaml.org/start.html)
- [kustomoidut JavaScript-objektit](https://www.w3schools.com/js/js_objects.asp)