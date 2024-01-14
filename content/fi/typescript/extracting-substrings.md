---
title:    "TypeScript: Alimerkkijonojen erottaminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi substringien erottelu on tärkeää

Substringien erottelu on tärkeä taito TypeScript-ohjelmoinnissa, koska se mahdollistaa merkkijonojen käsittelyn osissa sen sijaan, että käsiteltäisiin koko merkkijonoa. Tämä voi helpottaa koodin lukemista ja ymmärtämistä sekä optimoida suorituskykyä.

## Miten substringien erottelu tapahtuu

Substringien erottelu TypeScriptissä tapahtuu käyttämällä `substring()`-funktiota, joka palauttaa halutun osan merkkijonosta. Esimerkiksi seuraavan koodinpätkän avulla voidaan erottaa merkkijonosta "Tervetuloa!" ensimmäiset viisi merkkiä:

```TypeScript
let merkkijono = "Tervetuloa!";
let substring = merkkijono.substring(0,5);
console.log(substring); // Tulostaa "Terve"
```

Toinen tapa erottaa substring on käyttää `slice()`-funktiota, joka toimii samalla tavalla kuin `substring()`. Alla olevassa esimerkissä erottamalla merkkijonon kahdesta eri indeksistä saadaan sama tulos kuin edellisessä esimerkissä:

```TypeScript
let merkkijono = "Tervetuloa!";
let substring = merkkijono.slice(0,5);
console.log(substring); // Tulostaa "Terve"
```

## Syvällisempi katsaus substringien erotteluun

Molemmat `substring()` ja `slice()`-funktiot ottavat parametreikseen alkukohdan ja loppukohdan merkkijonossa, jonka perusteella palautetaan halutut merkit. `substring()` ei kuitenkaan hyväksy negatiivisia parametreja, joita slice voi hyväksyä.

On myös hyvä huomioida, että TypeScriptin merkkijonot indeksoidaan nollasta alkaen, joten esimerkiksi merkkijonon "Hei" ensimmäinen kirjain olisi indeksissä 0 ja viimeinen kirjain indeksissä 2.

## Katso myös

- [MDN Docs: substring()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Docs: slice()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/slice)