---
title:                "Tekstitiedoston lukeminen"
html_title:           "Javascript: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisin lukea tekstitiedostoa? Saatat miettiä. No hyvä kysymys!

Noh, voimme ensinnäkin käyttää tekstitiedostoja lukemiseen monessa eri tilanteessa. Haluatko lukea ja tarkistaa tiedot käyttäjän syötteestä? Siellä voit käyttää tekstitiedostoa. Tai ehkä haluat tallentaa pienen tekstimuotoisen tietokannan? Tekstitiedosto on loistava vaihtoehto. Yksinkertaisesti sanottuna, kyky lukea tekstitiedostoja on tärkeä taito jokaiselle Javascript-ohjelmoijalle.

## Miten

Ei ole yhtä tapaa lukea ja käsitellä tekstitiedostoja Javascriptillä, mutta tässä on yksi esimerkki, joka toimii hyvin useimmissa tilanteissa.

```Javascript
// Ensinnäkin, lataa tarvittava moduuli filesystem.
let fs = require('fs');

// Sitten luodaan muuttuja, joka säilyttää tiedoston polun.
let tiedostopolku = './tiedostot/tekstitiedosto.txt';

// Käytämme fs.readFile()-funktiota lukeaksemme tiedoston asynkronisesti.
fs.readFile(tiedostopolku, 'utf-8', (err, data) => {
  // Jos virhe, heitä virheilmoitus.
  if (err) throw err;
  
  // Muulloin tulostetaan luettu teksti konsoliin.
  console.log(data);
});
```

Tämä koodi lataa tiedoston path-muuttujan avulla ja käyttää sitten fs.readFile()-funktiota lukeakseen sisällön. Tulostamisen sijaan voit käsitellä dataa haluamallasi tavalla.

Esimerkkikoodin tulostus:

```Javascript
"Tämä on esimerkki tekstiä."
```

## Syvemmälle

Javascriptissa on useita tapoja lukea ja käsitellä tekstitiedostoja. Yllä olevassa esimerkissä käytimme fs-moduulia, mutta voit myös käyttää muita moduuleja, kuten readline tai stream. Voit myös käyttää muita funktioita, kuten fs.readFileSync(), joka lukee tiedoston synkronisesti ilman callbackia.

Tärkeintä on ymmärtää, miten tiedostojen polkuja käsitellään ja mitä parametreja ja metodeja on käytettävissä halutun toiminnan suorittamiseen. Kannattaa myös tutustua asynkronisten ja synkronisten funktioiden eroihin ja valita sopiva vaihtoehto sovelluksesi tarpeiden perusteella.

## Katso myös

- [fs moduuli Node.js dokumentaatiossa](https://nodejs.org/api/fs.html)
- [Readline käyttöohjeet](https://nodejs.org/api/readline.html)
- [Stream käyttöohjeet](https://nodejs.org/api/stream.html)
- [Asynkronisten ja synkronisten toimintojen vertailu](https://www.digitalocean.com/community/tutorials/understanding-the-node-js-event-loop-and-its-10x-faster-than-java-s)