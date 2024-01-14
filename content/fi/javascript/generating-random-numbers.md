---
title:                "Javascript: Satunnaisten numeroiden generointi."
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Uusimpana Javascript-ohjelmointimaailman trendinä on satunnaisten lukujen generoiminen. Tämän tekniikan avulla voit lisätä monimuotoisuutta ja jännitystä ohjelmiisi. Se voi myös olla hyödyllistä simulointeihin ja pelien kehittämiseen. 

## Miten

Satunnaisten lukujen generoiminen Javascriptilla on melko yksinkertaista. Käytämme siihen Math.random() -funktiota, joka palauttaa desimaaliluvun väliltä 0 (mukaan lukien) ja 1 (ei mukaan lukien). Voimme sitten kertoa tämän luvun halutulla välillä ja pyöristää sen haluttuun tarkkuuteen. Esimerkiksi, jos haluamme generoida kokonaislukuja väliltä 1-10, voimme käyttää seuraavaa koodia:

```Javascript
let satunnainenLuku = Math.random() * 10;
let tulos = Math.floor(satunnainenLuku) + 1;
console.log(tulos); // tulostaa satunnaisen luvun väliltä 1-10
```

Voit myös generoida satunnaisia merkkejä käyttämällä String.fromCharCode() -funktiota ja Math.floor() -funktiota. 

## Syvempi sukellus

Kaikki tietokonelaskelmat ovat periaatteessa ennustettavissa, joten satunnaislukujen generoiminen ei ole täysin sattumanvaraista. Math.random() käyttää taustalla tietokonelle siemenlukua, joka määrittää satunnaislukujen sarjan. Tämä tarkoittaa sitä, että jos annamme saman siemenluvun, huomaamme saman satunnaislukujen sarjan. Tämän vuoksi on tärkeää valita monimutkainen ja ainutlaatuinen siemenluku, kuten aikaleima tai satunnainen merkkijono, jotta saamme mahdollisimman sattumanvaraisia lukuja.

Voit myös käyttää muita Javascriptin esiasennettuja funktioita, kuten Date.now() ja Math.sin(), generoidaksesi satunnaisia lukuja, mutta tarvitaan lisätyötä niiden muuntamiseksi sopiviksi käyttöön. 

## Katso myös

- [MDN dokumentaatio Math.random() -funktiosta](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Tietokonelehti Tieteen Kuvalehden artikkeli satunnaisten lukujen generoimisesta](https://tieku.fi/tekniikka/kasittelemattomat-luvut-tietokoneessa-ruokinta-estaa-yksinkertaisen-geometrian-haittavaikutukset)
- [Javascript Generators -oppimisalusta](https://javascript.info/generators)