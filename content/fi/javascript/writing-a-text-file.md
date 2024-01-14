---
title:                "Javascript: Tiedoston kirjoittaminen"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla teksti-tiedoston voi tallentaa tietoja pysyvään muotoon ja käyttää niitä myöhemmin. Se on erittäin hyödyllistä esimerkiksi, jos haluat tallentaa listan tehtävistäsi tai tallentaa muistiinpanoja projektillesi.

## Kuinka kirjoittaa teksti-tiedosto

```Javascript
// Luodaan uusi tiedosto nimellä "teksti.txt"
let tiedosto = fs.createWriteStream("teksti.txt");

// Kirjoitetaan sisältö tiedostoon käyttäen "write" -metodia
tiedosto.write("Tämä on teksti tiedostoon.");

// Varmistetaan, että tiedostoon tallennettiin oikea sisältö
tiedosto.on("finish", function() {
	console.log("Teksti-tiedosto on tallennettu onnistuneesti.");
});

// Suljetaan tiedosto
tiedosto.end();
```

- Tämä esimerkki käyttää Node.js-kirjastoa, joten varmista, että olet asentanut sen ennen kuin kokeilet koodia.
- "fs" kirjaston avulla voit luoda, lukea ja kirjoittaa tiedostoja.
- Käytämme "createWriteStream" -metodia luomaan uuden tiedoston ja "on" -metodia varmistaaksemme, että kirjoittaminen on valmis.
- Lopuksi "end" -metodilla suljetaan tiedosto.

## Syventävä tieto teksti-tiedostojen kirjoittamisesta

Teksti-tiedostot voivat olla hyödyllisiä monissa ohjelmoinnin yhteyksissä, kuten tiedostojen luomisessa ja käsittelyssä sekä datan tallentamisessa. Kirjoittaessa teksti-tiedostoon, on tärkeää huomioida muutamia asioita:

- Muista aina sulkea tiedosto lopuksi.
- Voit käyttää "writeFileSync" -metodia, jos haluat kirjoittaa tiedoston synkronisesti.
- Teksti-tiedostoja voi lukea myös käyttäen "readFile" ja "readFileSync" -metodeja.

## Katso myös

- [Node.js dokumentaatio - tiedostojen kirjoittaminen](https://nodejs.org/api/fs.html#fs_fs_createwritestream_path_options)
- [W3Schools - Node.js tiedoston kirjoittaminen](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Freecodecamp - kuinka luoda ja kirjoittaa tiedosto Node.js:ssä](https://www.freecodecamp.org/news/node-js-tutorial-fs-module-file-system-beginners-guide/)