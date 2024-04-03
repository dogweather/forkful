---
date: 2024-01-27 20:35:50.828627-07:00
description: "Satunnaislukujen tuottaminen TypeScriptill\xE4 tarkoittaa ennalta arvaamattomien\
  \ numeeristen arvojen luomista m\xE4\xE4ritetyll\xE4 v\xE4lill\xE4. Ohjelmoijat\
  \ hy\xF6dynt\xE4v\xE4t\u2026"
lastmod: '2024-03-13T22:44:56.311377-06:00'
model: gpt-4-0125-preview
summary: "Satunnaislukujen tuottaminen TypeScriptill\xE4 tarkoittaa ennalta arvaamattomien\
  \ numeeristen arvojen luomista m\xE4\xE4ritetyll\xE4 v\xE4lill\xE4."
title: Satunnaislukujen generointi
weight: 12
---

## Mikä & Miksi?

Satunnaislukujen tuottaminen TypeScriptillä tarkoittaa ennalta arvaamattomien numeeristen arvojen luomista määritetyllä välillä. Ohjelmoijat hyödyntävät näitä satunnaislukuja monenlaisiin tarkoituksiin, kuten uniikkien tunnisteiden generointiin, datan simulointiin testaustarkoituksissa tai lisäämään arvaamattomuutta peleihin ja simulaatioihin.

## Kuinka:

TypeScriptissä voit luoda satunnaislukuja globaalin `Math`-objektin avulla. Alla on joitakin käytännön esimerkkejä, jotka osoittavat, kuinka tuottaa satunnaislukuja eri tarpeisiin.

### Perussatunnaisluvun tuottaminen

Perussatunnaisen desimaaliluvun tuottamiseksi välillä 0 (mukaan lukien) ja 1 (ei mukaan lukien) käytetään `Math.random()`-metodia. Tämä ei vaadi lisäkäsittelyä:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Tämä saattaa tulostaa arvon kuten `0.8995452185604771`.

### Satunnaisen kokonaisluvun tuottaminen kahden arvon välillä

Kun tarvitset kokonaislukua kahden tietyn arvon välillä, yhdistät `Math.random()`-metodin ja jonkin verran aritmetiikkaa:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Tämä saattaa tulostaa kokonaisluku arvon 1 ja 10 välillä, kuten `7`.

### Uniikin tunnisteen luominen

Satunnaislukuja voidaan yhdistää muihin menetelmiin uniikkien tunnisteiden luomiseksi, esimerkiksi yksinkertainen UUID-generaattorin koodinpätkä:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

Tämä luo merkkijonon, joka muistuttaa UUID:tä, kuten `110e8400-e29b-41d4-a716-446655440000`.

## Syväsukellus

Päämenetelmä satunnaislukujen generointiin JavaScriptissä ja näin ollen TypeScriptissä, `Math.random()`, nojaa pseudo-satunnaislukugeneraattoriin (PRNG). On tärkeää huomata, että vaikka tulokset saattavat vaikuttaa satunnaisilta, ne generoidaan deterministisellä algoritmilla perustuen alkuperäiseen siemenarvoon. Siksi `Math.random()`-metodilla tuotetut luvut eivät ole todella satunnaisia ja niitä ei tulisi käyttää kryptografisiin tarkoituksiin.

Kryptografisesti turvatut satunnaisluvut, Web Crypto API tarjoaa `crypto.getRandomValues()`-metodin, joka on saatavilla Web Crypto -standardia tukevissa ympäristöissä, mukaan lukien modernit selaimet ja Node.js (käyttäen `crypto`-moduulia). Tässä on nopea esimerkki sen käytöstä TypeScriptissä turvallisen satunnaisluvun generointiin tietyllä välillä:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Tämä menetelmä tarjoaa vahvemman satunnaisuuden tason ja sopii paremmin turvallisuusherkkiin sovelluksiin. Se on kuitenkin myös resurssi-intensiivisempi ja ei välttämättä ole tarpeellinen yksinkertaisempia tehtäviä, kuten yksinkertaisia simulaatioita tai ei-kriittistä satunnaisarvojen generointia varten.
