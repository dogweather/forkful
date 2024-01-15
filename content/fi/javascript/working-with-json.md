---
title:                "Työskentely jsonin kanssa"
html_title:           "Javascript: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON (JavaScript Object Notation) on yksi suosituimpia tiedostomuotoja, jota käytetään datan tallentamiseen ja siirtämiseen JavaScript-sovelluksissa. JSON:in avulla voidaan helposti muuntaa JavaScript-olioita tekstimuotoon ja päinvastoin, mikä tekee siitä erittäin hyödyllisen työkalun kehittäjille.

## Miten JSON toimii

```Javascript
// Luo JSON-objekti
const data = {
    name: "Matti",
    age: 25,
    hobbies: ["urheilu", "kirjoittaminen", "valokuvaus"]
};

// Muunna JSON tekstimuotoon
const jsonData = JSON.stringify(data);

// Tulostaa '{ "name": "Matti", "age": 25, "hobbies": ["urheilu", "kirjoittaminen", "valokuvaus"] }'
console.log(jsonData);

// Muunna tekstimuoto takaisin JSON-objektiksi
const parsedData = JSON.parse(jsonData);

// Tulostaa JavaScript-objektin { name: "Matti", age: 25, hobbies: ["urheilu", "kirjoittaminen", "valokuvaus"] }
console.log(parsedData);
```

JSON:in tärkein ominaisuus on sen yksinkertaisuus ja helppokäyttöisyys. Se koostuu avain-arvo pareista ja listoista, ja sen tekstimuotoinen rakenne on helposti luettavissa ihmisille ja tulkittavissa JavaScript:in avulla.

## Syventävä tieto JSON:ista

JSON tukee erilaisia tietotyyppejä, kuten merkkijonoja, numeroita, boole-arvoja ja listoja. Se myös tukee monenlaisten tietojen rakenteita, kuten objekteja ja listoja sisäkkäin. JSON on myös lähdekoodista riippumaton, mikä tarkoittaa sitä, että se toimii hyvin monissa erilaisissa ohjelmointikielissä.

Yksi tärkeä asia, jota kehittäjän tulisi tietää JSON:ista, on sen käyttö web-sovelluksissa. JSON voidaan nimittäin käyttää tiedon siirtoon clientin ja serverin välillä esimerkiksi AJAX-pyyntöjen yhteydessä. Se on myös yksi tärkeimmistä tiedostomuodoista, joita käytetään REST APIen kanssa kommunikoinnissa.

## Katso myös

- [JSON:n virallinen dokumentaatio](https://www.json.org/json-fi.html)
- [MDN:n opas JSON:in käyttöön](https://developer.mozilla.org/fi/docs/Learn/JavaScript/Objects/JSON)
- [JSON:in käyttö REST APIen kanssa](https://developer.mozilla.org/fi/docs/Web/HTTP/Methods/POST)