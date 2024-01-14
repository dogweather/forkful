---
title:                "TypeScript: Töitä csv-tiedostojen kanssa"
simple_title:         "Töitä csv-tiedostojen kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV-tiedostot ovat yleisiä tietojen tallentamiseen ja jakamiseen. Usein näitä tiedostoja käytetään esimerkiksi liiketoimintatiedon tallentamiseen tai Excel-taulukoiden siirtämiseen ohjelmistojen välillä. TypeScriptin avulla voit helposti lukea, kirjoittaa ja manipuloida CSV-tiedostoja. Tässä blogikirjoituksessa kerron tarkemmin, kuinka CSV-tiedostojen käsittely onnistuu TypeScriptillä.

## Kuinka

Aloita asentamalla `csv-parse`-paketti komennolla `npm install csv-parse`. Tämä paketti antaa meille tarvittavat työkalut CSV-tiedostojen lukemiseen. Seuraavaksi, voimme luoda yksinkertaisen TypeScript-tiedoston, joka lukee CSV-tiedoston ja tulostaa sen sisällön konsolille.

```TypeScript
import * as fs from 'fs';
import * as csv from 'csv-parse';

// Lue tiedosto ja muunna sen sisältö taulukoksi
fs.readFile('./data.csv', 'utf8', (err, data) => {
  csv.parse(data, (err, output) => {
    // Tulosta taulukon sisältö konsolille
    console.log(output);
  });
});
```

Käytämme tässä `fs`-moduulia lukemaan tiedostoa ja `csv-parse`-moduulia muuntamaan tiedoston sisällön taulukoksi. Kun ajamme koodin, näemme konsolilla CSV-tiedoston sisällön taulukkomuodossa.

```
[
  [ 'id', 'nimi', 'pisteet' ],
  [ '1', 'Matti', '350' ],
  [ '2', 'Anna', '500' ],
  [ '3', 'Jussi', '250' ]
]
```

Voimme myös kirjoittaa CSV-tiedoston käyttäen `csv`-moduulin `stringify`-funktiota. Alla olevassa esimerkissä luomme uuden CSV-tiedoston ja lisäämme siihen uuden rivin.

```TypeScript
import * as fs from 'fs';
import * as csv from 'csv-parse';

// Luo uusi rivi
const newRow = ['4', 'Liisa', '400'];

// Lue tiedosto ja muunna sen sisältö taulukoksi
fs.readFile('./data.csv', 'utf8', (err, data) => {
  csv.parse(data, (err, output) => {
    // Lisää uusi rivi taulukkoon
    output.push(newRow);

    // Kirjoita uusi taulukko CSV-tiedostoon
    const newCsv = csv.stringify(output);
    fs.writeFile('./data_new.csv', newCsv, (err) => {
      console.log('CSV-tiedosto luotu!');
    });
  });
});
```

Kun ajamme tämän koodin, näemme uuden CSV-tiedoston, jossa on lisätty uusi rivi.

```
id,nimi,pisteet
1,Matti,350
2,Anna,500
3,Jussi,250
4,Liisa,400
```

## Syvällinen sukellus

Nyt kun olet saanut perusasiat hallintaan, voit tutustua `csv-parse`-paketin dokumentaatioon löytääksesi kaikki sen tarjoamat toiminnot ja ominaisuudet. Voit esimerkiksi määrittää oman erottimen, jos CSV-tiedostossa on poikkeuksellinen erottimen käyttö. Voit myös hyödyntää callback-funktioita ja tapahtumankäsittelijöitä saadaksesi lisää kontrollia CSV-tiedostojen käsittelyssä.

## Katso myös

- [csv-parse paketin dokumentaatio](https://csv.js.org/parse/)
- [Node.js dokumentaatio](https://nodejs.org/fi/)
- [TypeScript-tutoriaalit](https://www.typescriptlang.org/docs/handbook/typescript-from-scratch.html)