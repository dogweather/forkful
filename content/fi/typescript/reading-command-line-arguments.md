---
title:                "TypeScript: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrien lukeminen on tärkeä taito jokaiselle ohjelmoijalle. Se mahdollistaa ohjelman suorittamisen erilaisten parametrien kanssa, mikä helpottaa ohjelman käyttöä ja tarjoaa enemmän toiminnallisuutta. Tässä blogikirjoituksessa opit lukemaan komentoriviparametreja TypeScriptillä ja saat syvempää tietoa tästä tärkeästä taidosta.

## Miten

Komentoriviparametrien lukeminen TypeScriptillä on helppoa. Seuraavassa koodilohkossa näet yksinkertaisen esimerkin, jossa tulostetaan komentoriviltä ohjelmalle annetun parametrin arvo.

```TypeScript
const parametri = process.argv[2];
console.log("Komentoriviparametri oli: " + parametri);
```

Jos suoritat tämän koodin komentoriviltä komennolla "ts-node index.ts argumentti", ohjelma tulostaa "Komentoriviparametri oli: argumentti".

Voit myös käyttää komentoriviparametreja ohjelman logiikan säätämiseen. Seuraavassa esimerkissä ohjelma tulostaa eri viestin sen mukaan, mikä parametri sille annetaan.

```TypeScript
const parametri = process.argv[2];
if (parametri === "tervetuloa") {
    console.log("Tervetuloa!");
} else if (parametri === "hei") {
    console.log("Hei!");
} else {
    console.log("Et antanut oikeaa parametria");
}
```

Jos suoritat tämän koodin komentoriviltä komennolla "ts-node index.ts hei", ohjelma tulostaa "Hei!".

Voit myös käyttää komentoriviparametreja ohjelman toiminnallisuuden laajentamiseen. Esimerkiksi voit antaa ohjelmalle oletusasetuksia komentoriviltä, mikä helpottaa sen käyttöä. Seuraavassa koodilohkossa ohjelmalle annetaan oletusarvona nimi, joka tulostetaan jos parametria ei anneta.

```TypeScript
const parametri = process.argv[2] || "Maailma";
console.log(`Hei ${parametri}!`);
```

Jos suoritat tämän koodin komentoriviltä komennolla "ts-node index.ts", ohjelma tulostaa "Hei Maailma!".

## Syvempää tietoa

Komentoriviparametrien lukeminen TypeScriptillä perustuu Node.js:n process-objektin argv-muuttujaan. Tämä muuttuja sisältää kaikki komentoriviparametrit taulukkona, jossa ensimmäinen parametri on aina polku Node.js:n suoritettavaan tiedostoon ja toinen parametri ohjelman nimi. Näiden jälkeen tulevat käyttäjän antamat parametrit.

Voit myös käyttää npm-pakettia yargs helpottamaan komentoriviparametrien lukemista. Yargs tarjoaa monipuolisia ja helppokäyttöisiä työkaluja komentoriviparametrien käsittelyyn. Tästä löydät lisätietoa yargsista ja siitä, kuinka voit hyödyntää sitä TypeScript-projekteissasi.

## Katso myös

- [Node.js: process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [Yargs](https://www.npmjs.com/package/yargs)