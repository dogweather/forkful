---
title:                "Sattumanvaraisten numeroiden generointi"
html_title:           "Javascript: Sattumanvaraisten numeroiden generointi"
simple_title:         "Sattumanvaraisten numeroiden generointi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit luoda satunnaisia numeroita ohjelmoidessasi? Yksinkertaisesti sanottuna, satunnaiset numerot ovat hyödyllisiä monissa tilanteissa, kuten pelien kehittämisessä, tietojen arvaamisessa tai salausavainten luomisessa.

## Miten

```Javascript
// Luodaan satunnainen kokonaisluku 1 ja 10 välillä
let luku = Math.floor(Math.random() * 10) + 1;
console.log(luku);

// Luodaan satunnainen desimaaliluku 0 ja 1 välillä
let desimaaliluku = Math.random();
console.log(desimaaliluku);

// Luodaan satunnainen kokonaisluku 10 ja 100 välillä
let suurempiLuku = Math.floor(Math.random() * 91) + 10;
console.log(suurempiLuku);
```

Esimerkki tulostuksesta:

```
7
0.390536452
84
```

`Math.random()`-funktion avulla voit luoda satunnaisia desimaalilukuja välillä 0 <= x < 1. Mutta jos haluat satunnaisia kokonaislukuja tietyllä välillä, voit käyttää `Math.floor()`-funktiota yhdistämällä se `Math.random()`-funktion kanssa.

## Syventävä tieto

Satunnaisia numeroita generoidessa on tärkeää varmistaa, että ne ovat mahdollisimman satunnaisia ja tasaisesti jakautuneita. `Math.random()`-funktio käyttää pseudosatunnaista algoritmia, joka perustuu tiettyyn aloitusarvoon, jotta se voi simuloida satunnaisuutta. Tämän vuoksi on suositeltavaa käyttää omaa algoritmia, joka perustuu todelliseen satunnaisuuteen.

## Katso myös

- [JavaScriptin Math-objekti W3Schoolsissa](https://www.w3schools.com/js/js_math.asp)
- [Miten luodaan satunnaisia numeroita JavaScriptissä MDN:ssä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)