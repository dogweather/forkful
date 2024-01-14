---
title:                "Javascript: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi lukea tiedostoa Javascript-ohjelmointia varten?

Tiedostojen lukeminen on tärkeä osa ohjelmointia, sillä se antaa mahdollisuuden käsitellä suuria määriä dataa sekä tallentaa ja lukea tiedostoja. Tämä on oleellista esimerkiksi silloin, kun halutaan tallentaa käyttäjän syöttämiä tietoja tai lukea ulkoisia resursseja, kuten kuva- tai tekstitiedostoja.

## Kuinka lukea tekstitiedostoa Javascriptillä?

Tiedostojen lukeminen Javascriptillä onnistuu käyttäen Node.js:ää tai selainpohjaista JS-ympäristöä. Seuraavassa on esimerkki koodista, jolla voit lukea ja tulostaa tekstitiedoston sisällön selainkonsolille:

```Javascript
// Luodaan uusi XMLHttpRequest-olio
var xhr = new XMLHttpRequest();

// Määritetään tapahtumankäsittelijä saadulle vastaukselle
xhr.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
        // Tulostetaan vastauksen tekstisisältö konsolille
        console.log(xhr.responseText);
    }
};

// Avataan yhteys ja lähetetään GET-pyyntö tiedostolle
xhr.open("GET", "esimerkki.txt", true);
xhr.send();
```
Esimerkkitiedosto "esimerkki.txt" sisältää esimerkkitekstiä, ja tämä koodi lukee sen ja tulostaa konsolille sen sisällön.

## Syväsukellus tiedostojen lukemiseen

Tiedostojen lukeminen voidaan toteuttaa myös käyttäen Node.js:ää ja sen sisäänrakennettua "fs" moduulia. Tämä mahdollistaa tiedoston lukemisen ja sisällön käsittelyn suoraan Javascript-tiedoston sisällä, esimerkiksi seuraavalla tavalla:

```Javascript
// Vaaditaan fs-moduuli
const fs = require('fs');

// Luetaan tiedoston sisältö muuttujaan
var sisalto = fs.readFileSync('esimerkki.txt', 'utf8');

// Tulostetaan sisältö konsolille
console.log(sisalto);
```

Tämä esimerkki lukee "esimerkki.txt" tiedoston sisällön ja tallentaa sen muuttujaan nimeltä "sisalto". Sen jälkeen se tulostaa sisällön konsolille.

## Katso myös

- [Node.js dokumentaatio](https://nodejs.org/en/docs/)
- [XMLHttpRequest-olio](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [fs-moduuli](https://nodejs.org/api/fs.html)