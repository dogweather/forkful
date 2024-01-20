---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen luominen on prosessi, jossa tuotetaan ennalta arvaamattomia numeroita. Ohjelmoijat tekevät tämän usein luodakseen satunnaisen käyttäjäkokemuksen tai simuloimaan tapahtumaa, jolla on satunnainen lopputulos.

## Miten:

Tässä on esimerkki siitä, kuinka tuotat satunnaislukuja TypeScriptillä:

```TypeScript
function getRandomInt(max: number) {
    return Math.floor(Math.random() * Math.floor(max));
}

console.log(getRandomInt(10));
```
Tämä tuottaa satunnaisen kokonaisluvun väliltä 0 ja 9. `Math.random()` palauttaa satunnaisluvun väliltä 0 (mukaan lukien) ja 1 (ei mukaan lukien), ja `Math.floor()` pyöristää alaspäin lähimpään kokonaislukuun.

## Syvälliset tiedot

Satunnaislukujen luonnissa on paljon historiallista kontekstia, joka ulottuu aina vanhoista roomalaisista (arpojat) digitaaliseen tietokoneaikaan. Nykyään on useita vaihtoehtoisia menetelmiä, kuten käyttöönottoon perustuva, algoritminen tai jopa kvanttisatunnainen.
TypeScriptin satunnaislukujen luominen pohjautuu JavaScriptin Math.random():iin , joka toteuttaa pseudo-satunnaislukugeneraattorin. Se on tarpeeksi hyvä useimpiin tarkoituksiin, mutta se ei ole riittävän vahva useimpiin kryptografiaan liittyviin tarkoituksiin, joissa on parempi turvautua platformispesifiseen satunnaislukugeneraattoriin, kuten window.crypto.getRandomValues() -selaimessa.

## Katso myös: 

Jos haluat laajentaa tietojasi satunnaislukujen luomisesta ja sen käytön eri  yhteyksissä, katso seuraavat linkit:

2. [MDN Math.random() documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
3. [Mozilla Quantum Random Number Generator](https://developer.mozilla.org/en-US/docs/Web/API/Window/crypto)