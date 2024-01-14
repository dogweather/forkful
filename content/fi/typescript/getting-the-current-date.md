---
title:                "TypeScript: Nykyisen päivämäärän hakeminen"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Ehkä olet joskus miettinyt, mikä päivämäärä on tänään. Ehkä sinulla on projekti, jossa tarvitset tietoa nykyisestä päivämäärästä. Tämä ohje auttaa sinua saamaan nykyisen päivämäärän TypeScript-koodisi avulla.

## Miten
Päivämäärän saamiseksi on olemassa useita erilaisia tapoja TypeScriptissä, mutta yksi yksinkertaisimmista on käyttää Date-objektia. Voit luoda uuden Date-objektin `new Date()`, joka antaa nykyisen päivämäärän ja ajan. Voit myös antaa halutun päivämäärän ja ajan parametreina `new Date(year, month, day, hours, minutes, seconds)`. Katso esimerkki alla olevassa koodilohkossa.

```TypeScript
// Luo uusi Date-objekti nykyisestä päivämäärästä
const nykyinenPaivamaara = new Date();

// Tulosta päivämäärä konsoliin
console.log(nykyinenPaivamaara);

// Luo uusi Date-objekti tiettyyn päivämäärään ja aikaan
const tulevaPaivamaara = new Date(2021, 11, 24, 18, 30, 0);

// Tulosta päivämäärä ja aika konsoliin
console.log(tulevaPaivamaara);
```

Koodin tulostama tulos olisi seuraava:

```
Mon Nov 16 2020 18:04:28 GMT+0200 (Eastern European Standard Time)
Fri Dec 24 2021 18:30:00 GMT+0200 (Eastern European Standard Time)
```

Voit myös käyttää Date-objektin erilaisia metodeita saadaksesi tarkempia tietoja päivämäärästä, kuten esimerkiksi `.getFullYear()` saadaksesi vuoden tai `.getMonth()` saadaksesi kuukauden numeron. Katso alla oleva esimerkki:

```TypeScript
// Hae vuosi nykyisestä päivämäärästä
const vuosi = nykyinenPaivamaara.getFullYear();

// Tulosta vuosi konsoliin
console.log(vuosi);

// Hae kuukauden numero (tammikuu on 0, joten lisää yksi)
const kuukausi = nykyinenPaivamaara.getMonth() + 1;

// Tulosta kuukausi konsoliin
console.log(kuukausi);
```

Yllä olevan koodin tulostama tulos olisi seuraava:

```
2020
11
```

## Syvimmät syövereet
Date-objekti perustuu UTC-aikaan (Coordinated Universal Time), joka on vakioaika ympäri maailman. Kun tulostat Date-objektin konsoliin, saat tulokseksi UTC-aian. Voit kuitenkin muuntaa sen haluamaasi aikaan käyttämällä Date-objektin `.toLocaleString()`-metodia ja antamalla haluamasi aikavyöhykkeen parametrina.

```TypeScript
// Aseta päivämääräksi tulevaPaivamaara
const nykyinenPaivamaara = tulevaPaivamaara;

// Muunna päivämäärä haluttuun aikavyöhykkeeseen
const muunnettuPaivamaara = nykyinenPaivamaara.toLocaleString('fi-FI', {timeZone: 'Europe/Helsinki'});

// Tulosta muunnettu päivämäärä konsoliin
console.log(muunnettuPaivamaara);
```

Yllä olevan koodin tulostama tulos olisi:

```
24.12.2021, 18:30:00
```

## Katso myös
- [Date-objektin dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScriptin viralliset