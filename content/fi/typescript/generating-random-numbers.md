---
title:                "Sattumanvaraisten lukujen generointi"
html_title:           "TypeScript: Sattumanvaraisten lukujen generointi"
simple_title:         "Sattumanvaraisten lukujen generointi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Ihmiset käyttävät satunnaislukuja monissa ohjelmoinnin tehtävissä, kuten lotonumeroiden arvonnassa, pelien luomisessa tai salasanojen generoinnissa. Satunnaislukujen avulla voidaan myös testata erilaisia skenaarioita ja ennustaa mahdollisia tapahtumia.

## Miten

Satunnaislukujen luominen TypeScriptissä on helppoa. Käytämme siihen `Math` -luokkaa ja sen tarjoamia metodeja.

```TypeScript
// Luo satunnaisluku väliltä 0-1
const randomNumber = Math.random();
console.log(randomNumber);

// Luo satunnaisluku väliltä 1-10
const randomInteger = Math.floor(Math.random() * 10) + 1;
console.log(randomInteger);

// Luo satunnaisluku väliltä 50-100
const randomRange = Math.floor(Math.random() * (100 - 50 + 1)) + 50;
console.log(randomRange);

// Luo satunnaisluku annetusta taulukosta
const fruits = ['apple', 'orange', 'banana', 'kiwi'];
const randomFruit = fruits[Math.floor(Math.random() * fruits.length)];
console.log(randomFruit);
```

**Tulostus:**

```
0.3749218685137936
6
84
orange
```

## Syväsukellus

`Math.random()` -metodi palauttaa desimaaliluvun välillä 0-1. Halutessamme generoida kokonaislukuja tai määrittää tietyn välialueen, käytämme apuna `Math.floor()` -metodia, joka pyöristää annetun luvun alaspäin lähimpään kokonaislukuun.

Satunnaislukujen luomisessa kannattaa myös huomioida, että kyseessä ei ole oikeasti täysin satunnaisia lukuja. Ne perustuvat tietokoneen kelloa jäljittelevään algoritmiin, joka tuottaa saman sarjan lukuja, jos sitä käytetään samassa järjestyksessä. Tämä tarkoittaa, että emme voi luottaa täysin satunnaisuuteen, etenkin jos generoimme suuren määrän lukuja.

## Katso myös

- [Official TypeScript documentation for Math class](https://www.typescriptlang.org/docs/handbook/standard-library.html#math)
- [Blog post: The dangers of relying on Math.random()](https://stackabuse.com/random-number-generation-in-javascript/)