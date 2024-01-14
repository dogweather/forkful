---
title:    "Javascript: Tekstitiedoston kirjoittaminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi kirjoittaisit tekstitiedoston Javascript-ohjelmointikielellä. Yksi syy voi olla tallentaaksesi tietoja käyttäjän toiminnasta tai luodaksesi lomakkeita verkkosivuille. 

## Miten

Aloittaaksesi tekstitiedoston kirjoittamisen Javascriptilla, tulee sinun luoda uusi tiedosto. Voit tehdä tämän käyttämällä Node.js:tä ja komentoa "fs.writeFile". Koodiblokissa näet esimerkin, kuinka voit luoda ja tallentaa tekstitiedoston nimeltä "uusi_tiedosto.txt".

```Javascript
const fs = require('fs'); 
fs.writeFile('uusi_tiedosto.txt', 'Tämä on uusi tekstitiedosto!', function (err) { 
	if (err) throw err; 
	console.log('Tiedosto tallennettu!'); 
});
```

Kun suoritat tämän koodin, luodaan uusi tekstitiedosto järjestelmään ja tallennetaan siihen annettu teksti. Voit myös lisätä muita parametreja, kuten merkkijonon enkoodauksen tai tarkistusfunktion tallentaaksesi tiedoston tiettyyn paikkaan.

## Syvällinen tarkastelu

Javascriptilla on myös muita tapoja kirjoittaa tekstitiedostoja, kuten "fs.appendFile" -komento, joka lisää tekstiä olemassa olevalle tiedostolle sen sijaan, että luodaan uusi tiedosto. Voit myös käyttää "fs.readFileSync" -komentoa lukeaksesi tiedoston sisältöä. 

On myös hyvä huomata, että tekstitiedostoissa voi olla erilaisia muotoiluja, kuten HTML tai CSV. Voit käyttää esimerkiksi "fs.createWriteStream" -komentoa luodaksesi virtaus kirjoittaaksesi HTML-tiedostoon.

## Katso myös

- [Codecademy - Writing Files in Node.js](https://www.codecademy.com/courses/introduction-to-javascript/lessons/file-io/exercises/writing-files)
- [W3Schools - Node.js File System Module](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [MDN Web Docs - Node.js file system documentation](https://developer.mozilla.org/en-US/docs/Learn/Server-side/Express_Nodejs/Introduction#File_system)