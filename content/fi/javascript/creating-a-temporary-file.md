---
title:    "Javascript: Väliaikaisen tiedoston luominen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

Väliaikaiset tiedostot ovat erittäin hyödyllisiä JavaScript-ohjelmoijille monissa eri tilanteissa. Ne voivat auttaa ratkaisemaan ongelmia, kuten tietojen tallentamista tilapäisesti tai ohjelman suorituksen aikana luotujen tiedostojen hallintaa. Väliaikaiset tiedostot myös parantavat koodisi suorituskykyä ja vähentävät turhien tiedostojen määrää laitteellasi.

## Miten luoda väliaikainen tiedosto?

Helpoin tapa luoda väliaikainen tiedosto JavaScript-ohjelmassa on käyttää `tmp` -moduulia. Voit asentaa tämän moduulin NPM:n avulla ja käyttää sitä koodissasi seuraavasti:

```javascript
const tmp = require('tmp');

tmp.file((err, path, fd, cleanupCallback) => {
  if (err) throw err;

  console.log('Temporary file:', path);
  
  // Use the temporary file here
  // ...

  // Delete the temporary file after using it
  cleanupCallback();
});
```

Tämä koodi luo automaattisesti väliaikaisen tiedoston, jonka polku tallennetaan `path`-muuttujaan. Voit käyttää tätä tiedostoa haluamallasi tavalla ja se poistetaan automaattisesti sen käytön jälkeen. Voit myös määrittää erilaisia vaihtoehtoja, kuten tiedoston nimen ja sijainnin, `tmp.file` -metodin parametreina.

## Syvempi sukellus

Väliaikaisen tiedoston luominen käyttäen `tmp` -moduulia käyttää oikeastaan vain Node.js:n `os` -moduulissa olevaa `tmpdir` -toimintoa. Tämä funktio palauttaa polun, jossa väliaikaiset tiedostot tallennetaan. Moduuli `tmp` vain abstraktoi tämän toiminnallisuuden ja tarjoaa kätevämmän API:n.

On myös tärkeää huomata, että väliaikaiset tiedostot ovat usein välttämättömiä, mutta ne eivät ole pitkäaikainen ratkaisu tietojen tallentamiseen. Jos tarvitset pysyvää tallennuspaikkaa, kannattaa harkita esimerkiksi tietokannan käyttöä.

## Katso myös

- [Tmp-moduulin dokumentaatio](https://www.npmjs.com/package/tmp)
- [Node.js:n os-moduulin dokumentaatio](https://nodejs.org/api/os.html#os_os_tmpdir)