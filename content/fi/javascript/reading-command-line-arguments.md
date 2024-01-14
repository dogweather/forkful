---
title:                "Javascript: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Miksi lukisit komentorivin argumentteja Javascript-ohjelmointikielellä? Komentorivin argumentit ovat hyödyllinen tapa lukea ja välittää tietoa ohjelmalle suoraan komentoriviltä. Tämä voi olla hyödyllistä, kun haluat mukauttaa ohjelmasi toimintaa tai suorittaa erilaisia tehtäviä komentoriviltä käsin.

## Miten
Komentorivin argumentit voidaan lukea ja käsitellä helposti Javascriptillä. Seuraavassa esimerkissä käymme läpi, kuinka voit lukea käyttäjän antaman komentorivin argumentin ja tulostaa sen konsoliin:

```Javascript
// Luodaan muuttuja, johon tallennetaan ensimmäinen komentorivin argumentti
let argumentti = process.argv[2];

// Tulostetaan vastaus konsoliin
console.log("Käyttäjä antoi komentorivin argumentin: " + argumentti);

// Komentorivillä syötetään esimerkiksi "node ohjelma.js Hello World"
// Tulostus: "Käyttäjä antoi komentorivin argumentin: Hello World"
```

Käyttämällä `process.argv` -oliota voit lukea kaikki komentorivin argumentit ja käsitellä niitä haluamallasi tavalla. Argumentit luetaan aina taulukkona, joten voit käyttää esimerkiksi `for`-silmukkaa niiden läpikäymiseen.

## Syvemmälle
Komentorivin argumentteja voidaan käyttää monipuolisesti ohjelmissa. Voit esimerkiksi muuttaa ohjelman toimintaa eri argumenttien perusteella tai välittää tietoa eri moduuleille ohjelman sisällä. On tärkeää muistaa, että komentorivin argumentit ovat aina merkkijonoja, joten tarvittaessa ne pitää muuttaa muuhun muotoon, esimerkiksi numeroksi, ennen käyttöä.

## Katso myös
- [process.argv dokumentaatio (englanniksi)](https://nodejs.org/dist/latest-v12.x/docs/api/process.html#process_process_argv)
- [Node.js ohjeet komentorivin argumenttien lukemiseen (englanniksi)](https://nodejs.dev/learn/nodejs-accept-arguments-from-the-command-line)
- [Komentorivin argumenttien hyödyntäminen Node.js:ssä (suomeksi)](https://code.visualstudio.com/docs/nodejs/nodejs-tutorial#_arguments)