---
title:                "Csv:n kanssa työskentely"
html_title:           "Javascript: Csv:n kanssa työskentely"
simple_title:         "Csv:n kanssa työskentely"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
CSV (Comma-Separated Values) on tiedostomuoto, jota käytetään yleensä taulukkomuotoisten tietojen tallentamiseen ja jakamiseen. Ohjelmoijat käyttävät sitä usein, koska se on yksinkertainen ja helppo lukea ja kirjoittaa tietoja.

## Kuinka tehdä:
- CSV-tietojen lukeminen:
```Javascript
const csvString = `Nimi, Ikä, Kaupunki
Maija, 27, Helsinki
Antti, 35, Turku`;

const csvData = csvString.split('\n').map(line => line.split(','));

console.log(csvData); // [["Nimi", "Ikä", "Kaupunki"], ["Maija", "27", "Helsinki"], ["Antti", "35", "Turku"]]
```
- CSV-tiedoston kirjoittaminen:
```Javascript
const csvData = [
  ['Nimi', 'Ikä', 'Kaupunki'],
  ['Maija', 27, 'Helsinki'],
  ['Antti', 35, 'Turku']
];

const csvString = csvData.map(row => row.join(',')).join('\n');

console.log(csvString); // Nimi, Ikä, Kaupunki
                         // Maija, 27, Helsinki
                         // Antti, 35, Turku
```

## Syvällisemmin:
- Historiallinen konteksti:
CSV muoto kehitettiin 1970-luvulla ja se oli alun perin tarkoitettu käytettäväksi osana IBM:n pääkirjanpitojärjestelmää. Nykyään se on yleinen ja suosittu tiedostomuoto erityisesti data-analytiikassa ja liiketoimintasovelluksissa.
- Vaihtoehtoja:
Vaikka CSV onkin yleinen ja helppokäyttöinen tiedostomuoto, se ei sovellu kaikkiin tietokoneohjelmiin. Esimerkiksi tietokannat ja JSON-muoto voivat olla parempia vaihtoehtoja, jos tietojen rakenteellinen muoto on tärkeä.
- Toteutusyksityiskohdat:
CSV-tiedostoja voidaan lukea ja kirjoittaa monilla eri ohjelmointikielillä, kuten esimerkiksi Pythonilla ja Javalla. Nämä kielet tarjoavat myös valmiita kirjastoja CSV-tiedostojen käsittelyyn.

## Katso myös:
- [Wikipedia-artikkeli CSV-muodosta](https://fi.wikipedia.org/wiki/CSV)
- [JSON-muodon vertailu CSV-muotoon](https://csveditor.com/fi/csv-vs-json)