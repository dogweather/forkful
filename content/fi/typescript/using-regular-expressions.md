---
title:    "TypeScript: Säännöllisten lausekkeiden käyttö"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi käyttäisit säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat hyödyllisiä ohjelmoinnissa, sillä ne mahdollistavat tietyn muotoisten merkkijonojen hakemisen ja/tai muokkaamisen. Ne ovat erityisen käteviä, jos sinun täytyy löytää useita samanlaisia merkkijonoja ja suorittaa tiettyjä toimenpiteitä niiden kanssa.

## Kuinka käyttää säännöllisiä lausekkeita TypeScriptissä

TS (TypeScript) tarjoaa sisäänrakennetun `RegExp` -luokan, joka mahdollistaa säännöllisten lausekkeiden käytön.

### Perustoiminnallisuudet

```TypeScript
// Luodaan säännöllinen lauseke, joka etsii kaikki "koira" -sanat
let regex = /koira/g;

// Testataan, sisältyykö säännöllisen lausekkeen vastine sanaan "koiranpentu"
console.log(regex.test("Tämä on söpö koiranpentu")); 
//Output: true

// Palautetaan kaikki säännöllisen lausekkeen vastineet merkkijonosta "minä rakastan koiria ja kissoja"
console.log("minä rakastan koiria ja kissoja".match(regex)); 
//Output: ['koira', 'koira']

// Vaihdetaan kaikki säännöllisen lausekkeen vastineet sanalla "kissa"
console.log("minä rakastan koiria ja kissoja".replace(regex, "kissa"));
//Output: "minä rakastan kissoja ja kissoja"
```

### Erikoismerkit

```TypeScript
// Käyttäjätunnuksen validointi (vain pienet kirjaimet ja numerot, 3-15 merkkiä pitkä)
let regex = /^[a-z\d]{3,15}$/;

// Testataan käyttäjätunnus "kissakisu88"
console.log(regex.test("kissakisu88")); 
//Output: true

// Testataan käyttäjätunnus "Koira1234"
console.log(regex.test("Koira1234")); 
//Output: false, koska isoja kirjaimia

// Testataan käyttäjätunnus "ma"
console.log(regex.test("ma")); 
//Output: false, koska liian lyhyt

// Testataan käyttäjätunnus "käyttäjä12"
console.log(regex.test("käyttäjä12")); 
//Output: false, koska välissä on erikoismerkkiä

// Vaihdetaan kaikki mainitut erikoismerkit pisteeksi
let regex = /[\-\_\+\.\@\=]/g;
console.log("kissa-sika+ankka@maa.fi".replace(regex, ".")); 
//Output: "kissa.sika.ankka.maa.fi"
```

## Syvällinen sukellus säännöllisiin lausekkeisiin

Säännölliset lausekkeet ovat erittäin monipuolisia ja niihin kuuluu paljon erilaisia erikoismerkkejä ja sääntöjä. Niiden käyttöön kannattaa tutustua perusteellisesti, jotta saat kaiken irti niiden tarjoamasta toiminnallisuudesta. Tässä muutamia hyödyllisiä resursseja:

- [TypeScript: Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [MDN: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regular Expressions 101](https://regex101.com/)

## Katso myös

- [TypeScriptin viralliset sivut](https://www.typescriptlang.org/)
- [TypeScriptin opas suomeksi](https://www.freecodecamp.org/news/learn-typescript-in-5-minutes-13