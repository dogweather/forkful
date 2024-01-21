---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:05:04.507924-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
Aloitetaan uusi projekti tarkoittaa puhtaalta pöydältä lähtöä, uuden sovelluksen tai kirjaston kehittämistä. Ohjelmoijat tekevät tämän, kun heillä on uusi idea tai tarve, jota ei voi tyydyttää olemassa olevilla ratkaisuilla.

## How to: (Kuinka tehdä:)
```TypeScript
// Asenna tarvittavat työkalut (esim. Node.js ja npm)
// Luo uusi kansio projektillesi ja navigoi sinne komentorivillä

// Initialisoi Node-projekti
$ npm init -y

// Asenna TypeScript
$ npm install typescript --save-dev

// Luo tsconfig.json tiedosto TypeScriptin konfigurointiin
$ npx tsc --init

// Luo ensimmäinen TypeScript-tiedosto, esimerkiksi index.ts
// Kirjoita koodisi
function moi(name: string): string {
  return `Moi, ${name}!`;
}

// Konsolille tulostuu: Moi, maailma!
console.log(moi('maailma'));
```

## Deep Dive (Sukellus syvyyksiin)
TypeScript luotiin vastaamaan JavaScriptin dynaamisuuden ja skaalautuvuuden haasteisiin. TypeScript on ollut olemassa vuodesta 2012, ja sen on kehittänyt Microsoft. Se laajentaa JavaScriptiä lisäämällä siihen tyypityksen.

Vaihtoehtoiset tapoja käynnistää projektiin voisi olla esimerkiksi JavaScript, Python tai Go kielellä. Jokaisella on omat vahvuutensa erilaisissa projekteissa. TypeScriptin valtti on sen kyky parantaa koodin laatua ja ylläpidettävyyttä tiukalla tyypityksellään.

Tärkeää projektin alussa on miettiä, onko tarvetta esimerkiksi frontend-kirjastolle (kuten React tai Angular) tai jos taas backend on huomion keskipisteessä, voisi valita Express.js:n tai NestJS:n, jotka toimivat sujuvasti TypeScriptin kanssa.

## See Also (Katso myös)
- [TypeScriptin viralliset dokumentit](https://www.typescriptlang.org/docs/)
- [npm:n dokumentaatio (projektin juurruttamiseksi)](https://docs.npmjs.com/cli/init)
- [Microsoftin TypeScript GitHub-repositorio](https://github.com/Microsoft/TypeScript)
- [TypeScript Deep Dive -ilmaiskirja](https://basarat.gitbook.io/typescript/)