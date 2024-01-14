---
title:    "Javascript: Satunnaislukujen luominen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi: Miksi käyttäisit JavaScriptiä satunnaisnumeroiden luomiseen?

JavaScriptillä on monia käyttötarkoituksia, kuten web-kehityksessä ja sovellusten luomisessa. Satunnaisnumeroiden luominen on yksi näistä käyttötarkoituksista. Satunnaisnumerot voivat olla hyödyllisiä esimerkiksi arpajaisten ja pelien yhteydessä, tai kun haluat testata koodia käyttämällä erilaisia satunnaisia arvoja.

## Miten: Esimerkkejä koodista ja tulosteista ```Javascript
// Luo satunnainen kokonaisluku välillä 1-100
let randomNumber = Math.floor(Math.random() * 100) + 1;
console.log(randomNumber);
// Tulostaa esimerkiksi: 46

// Luo satunnainen desimaaliluku välillä 0-1
let randomDecimal = Math.random();
console.log(randomDecimal);
// Tulostaa esimerkiksi: 0.723955611097

// Luo satunnainen merkkijono
let randomString = Math.random().toString(36).substr(2, 5);
console.log(`Salasana: ${randomString}`);
// Tulostaa esimerkiksi: Salasana: bcl1w

```

Voit myös käyttää muita JavaScriptin kirjastoja, kuten Chance.js tai Faker.js, joilla voit luoda erilaisia tietotyyppejä ja muokata satunnaisia arvoja tarkemmin.

## Syvällinen sukellus

Satunnaisnumeroiden luominen JavaScriptillä perustuu Math-objektiin, joka tarjoaa erilaisia metodeja satunnaisten arvojen luomiseen. Math.random() -metodi luo satunnaisen desimaaliluvun välillä 0-1. Tämän desimaaliluvun kertominen halutulla välillä ja lisääminen haluttuun lukumäärään antaa meille halutun alueen satunnaisia kokonaislukuja.

Voit myös käyttää muita metodeja, kuten Math.floor(), Math.ceil() ja Math.round(), jotka pyöristävät satunnaisia arvoja haluttuihin kokonaislukuihin. Lisäksi voit käyttää substring- ja toString-metodeja muokkaamaan satunnaisia arvoja ja luomaan esimerkiksi satunnaisia merkkijonoja salasanoille tai tunnisteille.

# Katso myös

- [MDN - Math](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [Chance.js](http://chancejs.com/)
- [Faker.js](https://github.com/marak/Faker.js/)