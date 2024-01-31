---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"

category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Työstämme CSV-tiedostoja JavaScriptilla, koska ne ovat helppolukuisia ja muokattavia datan tallennusmuotoja. CSV:tä käytetään usein tiedonvaihtoformaatissa, erityisesti silloin kun dataa siirretään eri ohjelmistojen välillä.

## How to:
JavaScriptilla CSV-tiedostojen käsittelyä varten käytetään usein `FileReader`-liittymää lukemaan tiedostot ja muuntoon `split()`- sekä `map()`-funktioita. Esimerkki CSV-tiedoston sisällön lukemisesta ja tulostamisesta:

```Javascript
// Lataa & lue CSV
const tiedostonLukija = new FileReader();

tiedostonLukija.onload = function(event) {
  const teksti = event.target.result;
  const rivit = teksti.split('\n').map(rivi => rivi.split(','));
  console.log(rivit);
};

// Tässä esimerkissä oletetaan, että käyttäjä on jo valinnut tiedoston.
tiedostonLukija.readAsText(valittuTiedosto);
```

Sample output:

```Javascript
[
  ["sarakkeen1_otsikko", "sarakkeen2_otsikko"],
  ["rivi1_sarakkeen1", "rivi1_sarakkeen2"],
  ["rivi2_sarakkeen1", "rivi2_sarakkeen2"],
  // ...
]
```

## Deep Dive
CSV-formaatti, joka tunnetaan myös "comma-separated values" -nimellä, on ollut käytössä jo 1970-luvulta lähtien. Se on säilynyt suosittuna, koska se on riittävän yksinkertainen ja tekstipohjainen. Nykyään on olemassa monia vaihtoehtoisia kirjastoja, kuten `PapaParse` ja `csv-parser`, jotka voivat helpottaa CSV-tiedoston käsittelyä. CSV-tiedostojen lukemista hankaloittaa toisinaan eri ohjelmien käyttämät eri rivinvaihtomerkit sekä sarakkeiden erotinmerkit.

## See Also
- [MDN FileReader Documentation](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- [PapaParse Library](https://www.papaparse.com/)
- [CSV-parser for Node.js](https://www.npmjs.com/package/csv-parser)
- [RFC 4180, CSV standard](https://tools.ietf.org/html/rfc4180)
