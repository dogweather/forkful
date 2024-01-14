---
title:                "Javascript: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi: Miksi käyttää satunnaisia lukuja ohjelmoinnissa?

Satunnaiset numerot ovat hyödyllisiä monissa eri sovelluksissa, kuten peleissä, arvontojen suorittamisessa ja simulaatioissa. Ne lisäävät myös kiinnostavuutta ja vaihtelua ohjelmointiin.

## Kuinka: Satunnaislukujen luominen JavaScriptillä

JavaScriptillä on valmiina toiminto Math.random(), joka palauttaa desimaaliluvun väliltä 0 (mukaan lukien) ja 1 (pois lukien). Voimme käyttää tätä toimintoa luomaan satunnaisia lukuja ja muuttaa niitä haluamallamme tavalla. Katso alla olevia esimerkkejä:

```Javascript 
// Luo satunnainen kokonaisluku väliltä 1-10
let luku = Math.floor(Math.random() * 10) + 1;

// Generoi satunnainen desimaaliluku väliltä 100-1000
let luku = Math.random() * (1000 - 100) + 100;

// Sekoita taulukon sisältö satunnaisesti
let taulukko = [1, 2, 3, 4, 5];
taulukko.sort(() => Math.random() - 0.5);

console.log(luku);
// Output vaihtelee esimerkeissä, esim. 5, 756.342, [3, 2, 5, 1, 4]
```

## Syvällinen tarkastelu: Satunnaislukujen generoiminen

Vaikka Math.random() toiminto on helppo ja nopea tapa luoda satunnaisia lukuja, se ei ole täysin sattumanvarainen. Se käyttää nimittäin tietokoneen kellon aikaa perustana ja luku ei ole täysin satunnainen, vaan se voidaan ennustaa. Tätä voidaan välttää seuraavien keinojen avulla:

- Käytä muita lähteitä, kuten hiiren liikettä tai näppäimistön käyttöä, luomaan lisävarmuutta generoituun lukuun.
- Muuta generoitu luku ennen sen käyttöä esimerkiksi käyttämällä muita matemaattisia funktioita, jotta luku ei ole ennalta arvattavissa.

Näiden keinojen avulla voimme lisätä satunnaisuutta ja turvallisuutta satunnaisia lukuja käytettäessä.

## Katso myös

- [MDN web docs: Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [W3Schools: Random Numbers](https://www.w3schools.com/js/js_random.asp)
- [FreeCodeCamp: How to Generate Random Numbers in JavaScript](https://www.freecodecamp.org/news/javascript-random-numbers-how-to-generate-them-even-more-efficiently/)