---
title:                "Työskentely jsonin kanssa"
html_title:           "TypeScript: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON (JavaScript Object Notation) on yleisesti käytetty tapa tallentaa ja vaihtaa tietoa.selvityssivu mikä sisältää, työtä ja sen tus ihmiset kuin ei työskentelevät PHP:n? Sitä käytetään usein web-sovelluksissa ja sen avulla voi lähettää tietoa esimerkiksi API-kutsujen välillä. JSON on myös helppo ymmärtää ja kirjoittaa, joten se on suosittu vaihtoehto tietojen tallentamiseen ja siirtämiseen.

## Miten

```typescript
const user = {
  name: "Matti",
  age: 28,
  hobbies: ["lukeminen", "lenkkeily", "valokuvaus"]
};

console.log(JSON.stringify(user));
// Output: {"name": "Matti", "age": 28, "hobbies": ["lukeminen", "lenkkeily", "valokuvaus"]}
```

Kuten näet, JSON-objektin luominen TypeScriptissä on helppoa. Voit käyttää erilaisia tietotyyppejä, kuten merkkijonoja, numeroita ja jopa taulukoita. Kun käytät JSON.stringify() -funktiota, voit muuntaa objektin helposti merkkijonoksi, joka on helppo siirtää ja tallentaa.

```typescript
const json = '{"name": "Anna", "age": 32, "hobbies": ["kirjoittaminen", "piirtäminen", "retkeily"]}';

const user = JSON.parse(json);

console.log(user.name);
// Output: Anna
```

JSON-tiedon purkaminen merkkijonosta takaisin objektiksi on myös yksinkertaista. Voit käyttää JSON.parse() -funktiota ja antaa sille merkkijonon muuttujana. Tämän jälkeen voit käyttää objektia kuten mitä tahansa muuta objektia.

## Syvällinen sukellus

JSON-tiedostojen käyttäminen TypeScriptissä on helppoa, mutta on tärkeää muistaa muutamia asioita. JSON-tiedostoissa käytetään aina kaksoislainausmerkkejä merkkijonojen ympärillä. Tästä syystä on tärkeää käyttää yksinkertaisia lainausmerkkejä objektin luomisessa, jotta vältytään virheiltä.

JSON-tiedostot koostuvat avain-arvo pareista, joissa käytetään kaksoispistettä erotin. Muista myös, että JSON-tietoja ei voi kommentoida, joten pidä koodi siistinä ja yksinkertaisena.

## Katso myös

- [JSON:n viralliset sivut] (https://www.json.org/json-fi.html)
- [TypeScriptin viralliset sivut] (https://www.typescriptlang.org/)