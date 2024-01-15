---
title:                "Kääntämisen otsikko: Työskentely csv:n kanssa"
html_title:           "TypeScript: Kääntämisen otsikko: Työskentely csv:n kanssa"
simple_title:         "Kääntämisen otsikko: Työskentely csv:n kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV (Comma-Separated Values) on yleinen tiedostomuoto, jota käytetään tietojen tallentamiseen ja jakamiseen. Se on helppolukuinen ja tukee monia eri ohjelmia ja sovelluksia. Tämän vuoksi on hyödyllistä tietää, kuinka työskennellä CSV-tiedostojen kanssa, jotta voi käsitellä tietoja tehokkaasti.

## Kuinka

Jos haluat lukea ja käsitellä CSV-tiedostoja TypeScriptin avulla, voit käyttää [Papaparse](https://github.com/mholt/PapaParse) -kirjastoa. Se tarjoaa kätevän tavan lukea ja muuntaa CSV-tiedostoja JavaScript-objekteiksi.

Asenna kirjasto ensin npm:llä komennolla `npm install papaparse`.

Sitten voit käyttää sitä TypeScriptin kanssa seuraavasti:

```TypeScript
// Importoi kirjasto
import * as Papa from 'papaparse';

// Luo muuttuja CSV-tiedoston sisällölle
const csv = `
Nimi, Ikä, Maa
Matti, 25, Suomi
Anna, 30, Ruotsi
`;

// Määritä CSV:n asetukset (valinnainen)
const options = {
  header: true // määrittelee, että ensimmäinen rivi sisältää otsikot kullekin sarakkeelle
};

// Käytä parse-funktiota lukemaan ja muuntamaan tiedot
const result = Papa.parse(csv, options);

// Tulosta tulos konsoliin
console.log(result.data); // tulostaa taulukon JavaScript-objekteja
```

Tämä koodi luo taulukon kolmesta JavaScript-objektista, joissa jokaisessa on kolme avainta: "Nimi", "Ikä" ja "Maa". Voit käyttää näitä tietoja ohjelmissasi haluamallasi tavalla.

## Syvempi sukellus

Papaparse-kirjasto tarjoaa myös paljon muita toimintoja, kuten tiedon kirjoittamisen CSV-muotoon, asetusten määrittämisen ja virheiden käsittelyn. Voit lukea lisää [kirjaston dokumentaatiosta](https://www.papaparse.com/docs).

CSV-tiedostot voivat myös sisältää monimutkaisempia tietoja, kuten upotettuja taulukoita ja rivien yhdistämistä. Tässä tapauksessa sinun on ehkä määriteltävä erilaiset asetukset tai käsiteltävä tiedot manuaalisesti. On hyödyllistä tuntea [CSV:n syntaksia](https://en.wikipedia.org/wiki/Comma-separated_values), jotta voit työskennellä kaikenlaisten tiedostojen kanssa.

## Katso myös

- [Papaparse-dokumentaatio](https://www.papaparse.com/docs)
- [CSV:n syntaksin opas](https://en.wikipedia.org/wiki/Comma-separated_values)