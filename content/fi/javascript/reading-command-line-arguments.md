---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Komentoriviargumentit ovat tietoja, joita ohjelma ottaa vastaan suoritettaessa. Niiden avulla ohjelmoijat voivat ohjata ja hallita ohjelman suorittamista käynnistyksen yhteydessä.

## Kuinka:

Voit lukea komentoriviargumentteja `process.argv`-objektin avulla. Tästä on esimerkki:

```Javascript
// prosessi.argv-testi.js
console.log(process.argv);
```

Kun suoritat tämän skriptin käyttämällä `node prosessi.argv-testi.js`, tulostuu seuraava:

```Javascript
[ '/usr/local/bin/node',
  '/home/user/prosessi.argv-testi.js' ]
```

Ne kaksi ensimmäistä arvoa ovat vakiot: Node.js-flgmentin polku ja polku skriptiin, jota suoritetaan. Kaikki argumentit, jotka lisätään skriptin suorituksen yhteydessä, tulevat näiden jälkeen.

## Deep Dive

Komentoriviargumenttien lukeminen on ollut osa ohjelmistokehitystä kauan ennen JavaScriptin ja Node.js:n tuloa. Se tarjoaa ohjelmoijalle suoran reitin ohjelman suoritusvirran kontrollointiin.

Vaihtoehtoinen tapa on käyttää yksittäisiä paketteja, kuten `commander` tai `yargs`, jotka auttavat argumenttien jäsentämisessä ja validoinnissa. 

Käytännön toteutuksessa `process.argv` on taulukko, jonka alkioita ovat suorituskomennon merkkijonot. Tämän ansiosta on mahdollista työskennellä argumenttien kanssa helposti ja joustavasti, kuten missä tahansa taulukossa.

## Katso Myös

Jos haluat oppia lisää komentoriviargumenttien käsittelystä, tutustu seuraaviin resursseihin:

- Node.js:n viralliset dokumentit: [process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- Opas komentoriviargumenttien työstämiseen [Stack Abuse](https://stackabuse.com/command-line-arguments-in-node-js/)
- Lisätietoja paketeista [commander](https://www.npmjs.com/package/commander) ja [yargs](https://www.npmjs.com/package/yargs) niiden NPM-sivuilta.