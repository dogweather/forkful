---
date: 2024-01-20 18:05:04.507924-07:00
description: "How to: (Kuinka tehd\xE4:) TypeScript luotiin vastaamaan JavaScriptin\
  \ dynaamisuuden ja skaalautuvuuden haasteisiin. TypeScript on ollut olemassa vuodesta\u2026"
lastmod: '2024-04-05T22:51:10.466221-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) TypeScript luotiin vastaamaan JavaScriptin dynaamisuuden\
  \ ja skaalautuvuuden haasteisiin."
title: Uuden projektin aloittaminen
weight: 1
---

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
