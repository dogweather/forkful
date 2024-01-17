---
title:                "Tiedostotiedoston kirjoittaminen"
html_title:           "Javascript: Tiedostotiedoston kirjoittaminen"
simple_title:         "Tiedostotiedoston kirjoittaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Kirjoittaminen tekstitiedostoon on tapa tallentaa tietoja ohjelmassa käytettäväksi myöhemmin. Ohjelmoijat voivat käyttää tätä tietoa esimerkiksi tallentaakseen käyttäjän antamia arvoja tai tulostaa tekstiä käyttäjän nähtäväksi myöhemmin.

# Kuinka tehdä?
Tässä on esimerkki siitä, kuinka voit kirjoittaa tiedon tekstitiedostoon käyttäen Javascriptia:

```Javascript
const fs = require('fs'); // Importataan tiedostojärjestelmä-moduuli

// Tiedon tallentaminen
const data = 'Tervetuloa tekstitiedostoon!';
fs.writeFile('teksti.txt', data, (err) => {
  if (err) throw err;
  console.log('Tiedot tallennettu onnistuneesti.');
});

// Tiedon lisääminen jo olemassa olevaan tiedostoon
const newData = '\nTämä on uusi rivi.';
fs.appendFile('teksti.txt', newData, (err) => {
  if (err) throw err;
  console.log('Tiedot lisätty onnistuneesti.');
});

// Tiedon lukeminen tiedostosta
fs.readFile('teksti.txt', (err, data) => {
  if (err) throw err;
  console.log(data.toString());
});
```

Tämän koodin tuloksena syntyy tiedosto nimeltä "teksti.txt", joka sisältää seuraavan tekstin:

```
Tervetuloa tekstitiedostoon!
Tämä on uusi rivi.
```

# Syväsukellus
Kirjoittaminen tekstitiedostoon on ollut tärkeä osa ohjelmointia jo pitkään. Aiemmin tätä toimintoa käytettiin enemmän, kun tiedot tallennettiin tietokoneen muistiin eikä pilvipalveluihin ollut vielä mahdollista tallentaa tietoja.

Javascriptissa on muitakin tapoja tallentaa tietoa, kuten esimerkiksi käyttäen JSON-tiedostoja tai verkkopalveluiden kautta. Kuitenkin tiedoston lukeminen ja kirjoittaminen suoraan tekstitiedostoon on edelleen hyödyllistä esimerkiksi yksinkertaisemmissa ohjelmissa.

Tiedon kirjoittaminen tapahtuu käyttäen Node.js:n fs-moduulia, joka tarjoaa erilaisia metodeja tiedon lukemiseen, kirjoittamiseen ja muokkaamiseen tiedostossa.

# Katso myös
Noden viralliset dokumentaatiot tiedoston lukemiseen, kirjoittamiseen ja muokkaamiseen: https://nodejs.org/dist/latest-v14.x/docs/api/fs.html