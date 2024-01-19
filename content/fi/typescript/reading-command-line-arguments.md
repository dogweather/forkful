---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Komentoriviparametrien lukeminen on prosessi, jossa ohjelma ottaa syötteitä suoraan komentoriviltä, jota ohjelmoija tai käyttäjä on syöttänyt. Ohjelmoijat tekevät tämän, koska se mahdollistaa ei-graafisten sovellusten suorittamisen ja säätämisen ilman erillistä käyttöliittymää.

## Näin se toimii:

TypeScriptillä me voimme käyttää Node.js:n 'process.argv' taulukkoa komentoriviparametrien lukemiseksi. Katsotaanpa lyhyt esimerkki:

```TypeScript
// Ohjelma lukee tämän käynnistyspolun ja mahdolliset parametrit
let myArgs = process.argv.slice(2);

// Tulostaa argumentit
console.log('Argumentit: ', myArgs);
```

Kun suoritat yllä olevan koodin komentoriviltä seuraavilla argumenteilla:

```bash
node myscript.ts 1 2 3
```

Saat tulostuksesi:

```
Argumentit: [ '1', '2', '3' ]
```

## Syvällisemmin:

(1) Historiallinen konteksti: varhaiset tietokoneohjelmat hyödynsivät komentorivisäätöjä, koska graafisia käyttöliittymiä ei ollut saatavilla. Tämä perinne on jatkunut uudempiin kieliin, kuten TypeScript. 

(2) Vaihtoehdot: Muutamat kirjastot, kuten 'yargs' ja 'commander', tarjoavat enemmän toimintoja ja mukavuutta työskennellä komentoriviparametrien kanssa. 

(3) Käytännön yksityiskohdat: 'process.argv' taulukko sisältää ensimmäisessä indeksissä polun Node.js:ään, toisessa indeksissä polun suoritettavaan tiedostoon, ja loput indeksit sisältävät komentoriviparametrit. Siksi 'process.argv.slice(2)' leikkaa pois kaksi ensimmäistä oliota, antaen meille vain parametrit.

## Katso myös:

- Yargs: [https://www.npmjs.com/package/yargs](https://www.npmjs.com/package/yargs)
- Commander: [https://www.npmjs.com/package/commander](https://www.npmjs.com/package/commander)
- Node.js prosessi-dokumentaatio: [https://nodejs.org/api/process.html#process_process_argv](https://nodejs.org/api/process.html#process_process_argv)