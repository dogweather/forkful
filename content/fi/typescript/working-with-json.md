---
title:                "TypeScript: Töitä tehdessä jsonin kanssa"
simple_title:         "Töitä tehdessä jsonin kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-json.md"
---

{{< edit_this_page >}}

Finlandin TypeScript ohjelmoijat, oletteko valmiita sukeltamaan JSONin maailmaan? JSON (JavaScript Object Notation) on yksi yleisimmin käytetyistä tiedostomuodoista web-kehityksessä. Tässä blogipostissa kerron teille, miksi ja miten voitte käyttää TypeScriptiä JSONin kanssa ja tarjoan myös syvällisempää tietoa aiheesta.

## Miksi

JSON on tullut erittäin suosituksi tiedostomuodoksi web-kehittäjien keskuudessa sen yksinkertaisuuden ja ihmisläheisyyden vuoksi. Se on myös helppo lukea ja ymmärtää sekä suorituskykyinen. JSONia käytetään yleisesti tietojen tallentamiseen ja siirtämiseen web-sovellusten ja palvelimien välillä.

## Miten

TypeScript on erinomainen valinta JSONin käsittelyyn sen vahvan tyyppisyyden vuoksi. Voit helposti parsia JSON-tiedoston TypeScript-olioksi käyttämällä sisäänrakennettua `JSON.parse()`-funktiota. Seuraavassa esimerkissä luodaan TypeScript-olio ja muutetaan se JSON-muotoon konsolin tulosteessa.

```TypeScript
const henkilö: { nimi: string, ikä: number } = {
  nimi: "Matti",
  ikä: 25
};

console.log(JSON.stringify(henkilö));
// tulostaa: {"nimi":"Matti","ikä":25}
```

Toisin kuin JavaScript, TypeScript voi auttaa tunnistamaan virheitä ennen ohjelman suorittamista, mikä tekee siitä luotettavamman vaihtoehdon JSONin käsittelyyn.

## Syväsukellus

JSON-muoto on melko yksinkertainen, mutta siinä on muutamia tärkeitä asioita, jotka on hyvä pitää mielessä. Ensinnäkin, JSON-tiedosto koostuu objekteista ja arvoista, jotka ovat joko merkkijonoja, numeroita, boolean-arvoja, taulukoita tai muita objekteja. Toiseksi, JSON-tiedostoissa käytetään usein `null`-arvoa, joka tarkoittaa "tyhjää".

JSON-tiedostot voi myös sisältää monenlaisia tietotyyppejä, mikä voi joskus aiheuttaa ongelmia jos ei ole tietoinen siitä, mitä tyyppiä odottaa. Esimerkiksi jos objektin arvo on arvo, joka voi olla joko merkkijono tai numero, TypeScript vaatii sinua määrittelemään sen `string | number` -tyypiksi. Tässä on hyvä käyttää TypeScriptin union-tyyppiä.

Näiden perustietojen lisäksi on hyödyllistä tutustua JSON-skeemaan, joka auttaa määrittelemään ja validoimaan JSON-tiedoston rakenteen. On myös olemassa erilaisia TypeScript-kirjastoja, jotka voivat auttaa käsittelemään JSONia, kuten `jsonschema`, `json2typescript` ja `typescript-json-schema`, joista jokaisella on omat ominaisuutensa ja tarkoituksensa.

## Katso myös

Jos haluat oppia lisää JSONin käsittelystä TypeScriptillä, tutustu seuraaviin resursseihin:

- [TypeScriptin virallinen dokumentaatio JSON-tyyppi](https://www.typescriptlang.org/docs/handbook/basic-types.html#json)
- [JSON-tyypit ja validointi TypeScriptissä](https://github.com/ajv-validator/ajv/blob/master/docs/typescript.md)
- [Erittäin yksityiskohtaiset TypeScript - JSON -vinkit](https://stijndewitt.com/2019/08/02/json-in-typescript-with-parameter-decorators)
- [Jokin ed