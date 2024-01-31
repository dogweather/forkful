---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "JSON-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
JSON eli JavaScript Object Notation on dataformaatti, jota käytetään tiedon tallentamiseen ja verkon kautta siirtämiseen. Ohjelmoijat käyttävät sitä siksi, että se on kevyt, helppolukuinen ja -kirjoitettava, ja useimmat ohjelmointikielet tukevat sitä, mikä tekee tiedon jakamisesta eri järjestelmien välillä vaivatonta.

## How to: (Kuinka tehdä:)
```Javascript
// JSON:in jäsentäminen
const jsonString = '{"name": "Koodari", "age": 25}';
const userObject = JSON.parse(jsonString);
console.log(userObject.name); // Tulostaa: Koodari

// JSON:ksi muuntaminen
const userDetail = { name: "Ohjelmoija", language: "JavaScript" };
const userJson = JSON.stringify(userDetail);
console.log(userJson); // Tulostaa: {"name":"Ohjelmoija","language":"JavaScript"}
```

## Deep Dive (Syväsukellus)
JSON juontuu JavaScript-maailmasta 2000-luvun alusta, mutta nykyään se on itsenäinen standardi. Vaihtoehtoina ovat esimerkiksi XML tai YAML, mutta JSON voittaa niitä keveydellään ja paremmalla luettavuudellaan. JSON:n käyttö on yksinkertaista: `parse` metodia käytetään merkkijonon muuntamisessa JS-objektiksi ja `stringify` JS-objektin muuntamisessa merkkijonoksi. Tämä tekee JSON:sta loistavan valinnan dataobjektien siirtämiseen esimerkiksi selaimen ja palvelimen välillä.

## See Also (Katso myös)
- Mozilla Developer Networkin JSON-ohjeet: [MDN JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- JSON-syntaksin yleiskatsaus: [JSON.org](http://json.org/)
