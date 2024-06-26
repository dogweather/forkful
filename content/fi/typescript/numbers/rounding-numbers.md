---
date: 2024-01-26 03:47:26.001825-07:00
description: "Kuinka: TypeScriptiss\xE4 py\xF6rist\xE4minen voidaan tehd\xE4 k\xE4\
  ytt\xE4en useita menetelmi\xE4. T\xE4ss\xE4 pikakatsaus."
lastmod: '2024-03-13T22:44:56.310466-06:00'
model: gpt-4-0125-preview
summary: "TypeScriptiss\xE4 py\xF6rist\xE4minen voidaan tehd\xE4 k\xE4ytt\xE4en useita\
  \ menetelmi\xE4."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
TypeScriptissä pyöristäminen voidaan tehdä käyttäen useita menetelmiä. Tässä pikakatsaus:

```typescript
// Math.round pyöristää lähimpään kokonaislukuun
console.log(Math.round(1.5)); // Tuloste: 2

// Math.ceil pyöristää ylöspäin lähimpään kokonaislukuun
console.log(Math.ceil(1.1)); // Tuloste: 2

// Math.floor pyöristää alaspäin lähimpään kokonaislukuun
console.log(Math.floor(1.8)); // Tuloste: 1

// toFixed pyöristää kiinteään desimaalilukumäärään
let num = 1.23456;
console.log(num.toFixed(2)); // Tuloste: "1.23"
// Huomio: toFixed palauttaa merkkijonon! Käytä parseFloatia muuntaaksesi takaisin tarvittaessa.
console.log(parseFloat(num.toFixed(2))); // Tuloste: 1.23
```

## Syväsukellus
Aikoinaan pyöristäminen oli välttämätöntä rajallisen tilan ja tarkkuusongelmien vuoksi varhaisissa tietokoneissa. Nykyään, liukulukuaritmetiikka voi johtaa omituisiin tuloksiin johtuen siitä, miten numerot tallennetaan binäärimuodossa. Pyöristämisen vaihtoehtoihin kuuluvat floor, ceil ja trunc (desimaalien katkaiseminen ilman pyöristämistä).

Sisäiset seikat ovat merkittäviä: `Math.round` noudattaa "pyöristä puoli ylös" -periaatetta (tunnetaan myös "kaupallisena pyöristämisenä"), kun taas `Math.floor` ja `Math.ceil` ovat suoraviivaisia. `toFixed` saattaa aiheuttaa odottamattomia tuloksia, koska se palauttaa merkkijonon, ja se pyöristää käyttäen "pyöristä puoli parilliseen" -periaatetta (tunnetaan myös "pankkiirin pyöristämisenä"), erityisen hyödyllinen vähentämään vinoumaa pyöristettäessä samoja numeroita useita kertoja.

## Katso Myös
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE-standardi liukulukuaritmetiikalle (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
