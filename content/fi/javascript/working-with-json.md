---
title:                "Javascript: Töissä jsonin kanssa"
simple_title:         "Töissä jsonin kanssa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON on tärkeä osa nykyaikaista ohjelmointia, sillä se tarjoaa tapa tallentaa ja jakaa tietoa verkon kautta. JSON-formaatti on myös erittäin yhteensopiva muiden ohjelmointikielten kanssa, joten sen kanssa työskentely on erittäin tärkeä taito.

## Kuinka

```Javascript
// Esimerkki JSON-objektista
const json = {
  "nimi": "Matti Meikäläinen",
  "ikä": 30,
  "harrastukset": ["joukkuelajit", "käsitöiden tekeminen", "kirjallisuus"]
}

// Muunna JSON-objekti tekstiksi
const teksti = JSON.stringify(json);
console.log(teksti); // tulostaa {"nimi": "Matti Meikäläinen", "ikä": 30, "harrastukset": ["joukkuelajit", "käsitöiden tekeminen", "kirjallisuus"]}

// Muunna JSON-teksti takaisin objektiksi
const objekti = JSON.parse(teksti);
console.log(objekt); // tulostaa {nimi: "Matti Meikäläinen", ikä: 30, harrastukset: ["joukkuelajit", "käsitöiden tekeminen", "kirjallisuus"]}
```

## Syvällinen sukeltaminen

JSON-objektit koostuvat avain-arvo -pareista, joita kutsutaan myös nimikkeiksi (keys) ja arvoiksi (values). JSON tukee useita eri tietotyyppejä, kuten merkkijonoja, numeroita, taulukoita ja muita objekteja. JSON-objektien käsittelyyn on olemassa lukuisia kirjastoja ja työkaluja, jotka voivat helpottaa työskentelyä JSON-datalla.

## Katso myös

- [MDN:n JSON-opas](https://developer.mozilla.org/fi/docs/Learn/JavaScript/Objects/JSON)
- [JSON:n virallinen verkkosivusto](https://www.json.org/json-fi.html)
- [JSON-objektien manipulointi jQuery-kirjastolla](https://www.w3schools.com/jquery/ajax_parsejson.asp)