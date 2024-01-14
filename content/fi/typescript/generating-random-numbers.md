---
title:                "TypeScript: Satunnaisten lukujen generointi"
simple_title:         "Satunnaisten lukujen generointi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi käyttää satunnaislukugenerointia TypeScriptissä
Satunnaislukujen generointi on tärkeä osa ohjelmointia monissa sovelluksissa, kuten pelit, simulaatiot ja salausmenetelmät. Satunnaislukujen avulla voidaan luoda ennustamattomia tapahtumia ja tehdä mahdottomia tehtäviä, jotka vaativat sattumanvaraisuutta. TypeScript tarjoaa helpon tavan generoida satunnaislukuja, joka on olennainen taito jokaiselle ohjelmoijalle.

## Näin generoit satunnaislukuja TypeScriptissä
Satunnaislukujen generoimiseksi TypeScriptissä voit käyttää Math-luokkaa, joka tarjoaa valmiita metodeja satunnaislukujen luomiseen. Esimerkiksi voit käyttää Math.random() -metodia, joka palauttaa desimaaliluvun välillä 0-1. Voit myös käyttää Math.floor() ja Math.ceil() -metodeita pyöristämään luku haluamaasi muotoon. Seuraava esimerkki näyttää, miten generoida satunnaisluku väliltä 1-10 ja tulostaa se konsoliin:

```TypeScript
const randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum);
```

Tämä koodi käyttää Math.random() -metodia luomaan desimaaliluvun väliltä 0-1 ja sitten Math.floor() -metodia pyöristämään sen alaspäin lähimpään kokonaislukuun. Seuraavaksi se lisää 1 luvun loppuun, jotta saadaan haluttu väli väliltä 1-10. Lopuksi tulostetaan satunnaisluku konsoliin.

## Syventyvä sukellus satunnaislukujen generointiin
Vaikka Math-luokka tarjoaa käteviä metodeja satunnaislukujen generointiin, niillä on omat rajoituksensa. Esimerkiksi, jos haluat generoida satunnaisen kokonaisluvun tietyltä väliltä, sinun täytyy käyttää erilaisia kaavoja ja metodeja päästäksesi haluttuun lopputulokseen. Voit myös käyttää algoritmeja, kuten Mersenne Twister, jotka tuottavat sattumanvaraisempia lukuja kuin Math-luokan valmiit metodit.

On myös hyvä huomata, että satunnaislukujen generoiminen on mahdotonta ilman ulkopuolista lähdettä, kuten ympäristön kohinaa tai tietokoneen kelloa. Tämä tarkoittaa, että vaikka satunnaislukuja voidaan pitää "sattumanvaraisina", ne ovat itse asiassa ennalta määrättyjä ja jäljitettäviä. Satunnaislukujen käyttöä salauksessa tuleekin harkita tarkkaan.

## Katso myös
- [Math-luokan dokumentaatio TypeScriptissä](https://www.typescriptlang.org/docs/handbook/stdlib.html#math)
- [Mersenne Twister -algoritmi](https://en.wikipedia.org/wiki/Mersenne_Twister)