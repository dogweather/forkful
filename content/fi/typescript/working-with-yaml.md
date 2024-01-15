---
title:                "Työskentely yaml:n kanssa"
html_title:           "TypeScript: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on helppo ymmärtää ja kirjoittaa, mikä tekee siitä suositun vaihtoehdon tietojen tallentamiseen ja siirtämiseen. Se on myös tarkoitettu erityisesti ohjelmointikielille ja helpottaa tietojen käsittelyä näissä ympäristöissä.

## Kuinka

```TypeScript
const yaml = require('js-yaml');
const fs = require('fs');

//Luodaan JSON objekti
const data = {
  customer: "John Smith",
  order: {
    id: "12345",
    products: ["Shirt", "Jeans", "Shoes"],
    total: 124.99
  }
}

//Muutetaan YAML-muotoon
const yamlData = yaml.safeDump(data);

//Kirjoitetaan tiedostoon
fs.writeFileSync('order.yaml', yamlData);

//Luetaan tiedostosta
const loadedData = yaml.safeLoad(fs.readFileSync('order.yaml', 'utf8'));

//Tulostetaan pois JSON-muodossa
console.log(loadedData);
```

Tämän esimerkin avulla voit luoda JavaScript-objektin ja tallentaa sen YAML-tiedostoon. Voit myös lukea YAML-tiedoston ja muuttaa sen takaisin JSON-muotoon. Tämä helpottaa tietojen siirtämistä ja käsittelyä eri ohjelmointikielillä.

## Syvällinen sukellus

YAML (YAML Ain't Markup Language) on kevyt ja ihmisen luettava tietokielijärjestelmä, joka perustuu avoimiin standardiarkistoihin. Sitä käytetään yleisesti konfiguraatiotiedostoina ja tiedon tallentamiseen. YAML-kieli erottuu muista merkintäkieleistä sen yksinkertaisuuden ja välitetyt tiedostojen selkeyden ja helposti luettavien muotoilujen avulla. Se on myös laajalti yhteensopiva monien ohjelmointikielien kanssa ja sitä käytetään laajasti sovellusten ja palvelinten konfiguroinnissa.

## Katso myös

- [YAML.org](https://yaml.org/)
- [js-yaml Kirjasto](https://www.npmjs.com/package/js-yaml)
- [JSON vs. YAML vertailu](https://www.baeldung.com/java-json-vs-yaml)