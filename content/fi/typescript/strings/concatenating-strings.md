---
date: 2024-01-20 17:35:39.169899-07:00
description: "Kuinka: Aikoinaan, JavaScriptiss\xE4 merkkijonojen yhdist\xE4minen tapahtui\
  \ vain `+` operaattorilla tai `concat()`-metodilla. TypeScript, JavaScriptin\u2026"
lastmod: '2024-04-05T21:53:57.864949-06:00'
model: gpt-4-1106-preview
summary: "Aikoinaan, JavaScriptiss\xE4 merkkijonojen yhdist\xE4minen tapahtui vain\
  \ `+` operaattorilla tai `concat()`-metodilla."
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## Kuinka:
```TypeScript
// Yksinkertainen yhdistäminen käyttäen + -operaattoria
let tervehdys = "Hei " + "maailma!";
console.log(tervehdys); // "Hei maailma!"

// Template-literalit sallivat muuttujien sijoittamisen merkkijonoihin
let nimi = "Pekka";
let tervehdysNimella = `Terve, ${nimi}!`;
console.log(tervehdysNimella); // "Terve, Pekka!"

// Array.join() menetelmä yhdistää merkkijonot taulukosta
let sanat = ["TypeScript", "on", "siistiä!"];
let lause = sanat.join(" ");
console.log(lause); // "TypeScript on siistiä!"
```

## Syväsukellus
Aikoinaan, JavaScriptissä merkkijonojen yhdistäminen tapahtui vain `+` operaattorilla tai `concat()`-metodilla. TypeScript, JavaScriptin yliluokka, seuraa samaa perinnettä, mutta tarjoaa lisäksi kirjaimellisen syntaksin (template literals), joka helpottaa dynaamista yhdistämistä. Vaihtoehtoja yhdistämiseen ovat `Array.join()` ja erilaiset kolmannen osapuolen kirjastojen funktiot. Suorituskykyyn vaikuttaa moni asia, kuten yhdistettävien merkkijonojen pituus ja määrä. Yleensä template-literalit ja `+` operaattori ovat tehokkaimpia pienissä operaatioissa, mutta massiivisissa yhdistelyissä kannattaa mitata suorituskyky tapauskohtaisesti.

## Katso Myös
- MDN Web Docs - merkkijonot ja merkkijonon käyttö [String - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
