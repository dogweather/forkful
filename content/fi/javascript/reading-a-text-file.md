---
title:                "Tiedostosta lukeminen"
html_title:           "Javascript: Tiedostosta lukeminen"
simple_title:         "Tiedostosta lukeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & miksi?
Lukeminen tekstiasiakirjasta on prosessi, jossa tietokone järjestelmä lukee ja käsittelee tekstiä tallennettuna tiedostona. Tämä on tärkeä osa ohjelmoinnin maailmaa, sillä se mahdollistaa tiedon saannin ja käsittelyn ohjelmien käytössä.

## Miten:
Alla on kaksi esimerkkiä siitä, kuinka lukea yksinkertainen tekstiasiakirja käyttäen Javascriptia. Seuraa koodiesimerkkejä niiden koodeja asentaaksesi tai suorittaaksesi sitä omalla koneellasi.

Esimerkki 1: Lukeminen ja tulostaminen yksinkertaisesta tekstiasiakirjasta:

```
const fs = require('fs');
 
fs.readFile('tekstiasiakirja.txt', 'utf8', function(error, data) {
    let lines = data.split("\n");
    lines.forEach(line => {
        console.log(line);
    });
});
```

Esimerkki 2: Tallentaminen tekstiasiakirjaan käyttäen käyttäjän syötettä:

```
const fs = require('fs');
const readline = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
});

readline.question('Kirjoita jotain: ', (userInput) => {
  fs.appendFile('tekstiasiakirja.txt', userInput, (err) => {
    if (err) throw err;
    console.log('Tallennettu!');
  });
  readline.close();
});
```

### Syöte:
```
Tämä on esimerkki tekstiä.
Tämä on toinen rivi.
```

### Tulos:
```
Tämä on esimerkki tekstiä.
Tämä on toinen rivi.
```

## Syväsukellus:
Tekstiasiakirjojen lukeminen ja kirjoittaminen on ollut osa ohjelmointia jo vuosikymmeniä. Aikaisemmin se tapahtui lähinnä käyttöjärjestelmän kautta, mutta nykyään ohjelmoijat voivat käyttää monia erilaisia kirjastoja ja työkaluja tähän tarkoitukseen. Joitakin vaihtoehtoja Javascriptillä lukemiseen ja kirjoittamiseen ovat muun muassa Node.js, fs-moduuli ja readline-kirjasto. Tekstiasiakirjat ovat myös osa monia tiedostonhallintajärjestelmiä ja verkkokäyttöliittymiä, joten niiden lukeminen ja kirjoittaminen on tärkeä taito jokaiselle ohjelmoijalle.

## Katso myös:
- Node.js: https://nodejs.org/en/
- fs-moduuli: https://nodejs.org/api/fs.html
- readline-kirjasto: https://nodejs.org/api/readline.html