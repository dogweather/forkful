---
title:                "Luenta komentorivin argumenteista"
html_title:           "Javascript: Luenta komentorivin argumenteista"
simple_title:         "Luenta komentorivin argumenteista"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrien lukeminen on tärkeä taito jokaiselle kehittäjälle, joka haluaa tehdä monipuolisia ja interaktiivisia ohjelmia käyttöjärjestelmän tasolla. Se antaa mahdollisuuden kommunikoida ohjelman kanssa käyttämällä syötteitä komentoriviltä työmme tehostamiseksi.

## Kuinka

Komentoriviparametrien lukeminen on helppoa Javascriptissä käyttämällä `process.argv` -muuttujaa. Tämä muuttuja sisältää taulukon kaikista käyttäjän antamista komentoriviparametreistä. Voit käyttää esimerkiksi for-silmukkaa tai `forEach` -metodia käydäksesi läpi kaikki parametrit ja työstääksesi niitä haluamallasi tavalla.

```javascript
process.argv.forEach((parametri) => {
  console.log(parametri);
});
```

Tämän koodin suorittaminen komentoriviltä antaisi seuraavan tulosteen, mikäli siihen lisättäisiin kolme parametria: `node index.js ekaTuloste tokaTuloste kolmasTuloste`

```
ekaTuloste
tokaTuloste
kolmasTuloste
```

Komentoriviparametreillä voi myös olla erilaisia ominaisuuksia, kuten esimerkiksi nimettyjä parametreja käytettäessä. Tällöin voit käyttää esimerkiksi `yargs` -pakettia auttamaan parametrien lukemisessa.

```javascript
// Asennetaan yargs-paketti ensin
npm install yargs

// Sitten voidaan käyttää sitä koodissa
const yargs = require('yargs');

// Määritellään parametrit
yargs.command({
  command: 'tervehdi',
  describe: 'Tulostaa käyttäjälle tervehdyksen',
  // Aliakset, joilla komento voidaan suorittaa
  aliases: ['tervehdiyttävä', 'hei'],
  builder: {
    // Tallennetaan annettu arvo yhteiseen muuttujaan
    nimi: {
      describe: 'Käyttäjän nimi',
      demandOption: true,
      // Pakotetaan annetun arvon olevan vähintään 3 merkkiä pitkä
      validate: (value) => {
        if (value.length < 3) {
          throw new Error('Nimen tulee olla vähintään 3 merkkiä pitkä.');
        }
      }
    }
  },
  handler: (argv) => {
    console.log('Hei ' + argv.nimi + '!');
  }
});

// Parsitaan parametrit
yargs.parse();
```

Suoritettaessa komento `node index.js tervehdi --nimi John` tulisi tulosteeksi `Hei John!`.

## Deep Dive

Komentoriviparametri voi myös olla tietynlainen lippu, joka ilmaisee tiettyä toiminnallisuutta. Tämän avulla voit tehdä ohjelmastasi interaktiivisemman ja antaa käyttäjän määrittää haluamansa toiminnot parametreilla.

```javascript
yargs.command({
  command: 'nimet',
  describe: 'Tulostaa käyttäjälle erilaisia nimiä',
  builder: {
    // Lisätään liput `-s` ja `-n`
    lyhyet: {
      describe: 'Haluatko lyhyitä nimiä?',
      // Yksinkertaisuuden vuoksi ei pakoteta arvoa, joten se voi olla joko true tai false
      default: false
    },
    numeroiden: {
      describe: 'Haluatko nimiin lisätä numeroita?',
      // Sama tässä, pakotetaan arvoksi true
      default: true
    }
  },
  handler: (argv) => {
    let nimet = ['Maija', '