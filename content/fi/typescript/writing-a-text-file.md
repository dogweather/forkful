---
title:    "TypeScript: Tekstitiedoston kirjoittaminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi
Tekstin tiedostoon kirjoittamisella voi olla monia syitä, kuten tallentaa tietoa pysyvästi tai jakaa tietoja muille käyttäjille. Teksti-tiedostojen kirjoittaminen on myös tärkeä osa ohjelmointia, sillä usein tarvitsemme tallentaa tietoja ja muokata niitä myöhemmin.

## Kuinka kirjoittaa teksti-tiedosto TypeScriptillä
Tekstin tallentaminen tiedostoon onnistuu TypeScriptin avulla helposti. Alla olevassa esimerkissä luomme muuttujan "teksti" ja käytämme "fs" -kirjastoa tallentamaan tekstin tiedostoon nimeltä "teksti.txt". 

```
TypeScript
// Esimerkki tallentamisesta tekstiä tiedostoon
let teksti = "Tämä on teksti-tiedoston sisältö!";
fs.writeFileSync("teksti.txt", teksti);
```

Voit tarkistaa, onnistuiko tekstin tallentaminen tarkistamalla tiedoston sisältö terminaalista tai avaamalla "teksti.txt" -tiedosto esimerkiksi tekstieditorilla. Näet, että teksti "Tämä on teksti-tiedoston sisältö!" on tallentunut tiedostoon.

## Deep Dive
Kun tallennat teksti-tiedostoon, on tärkeää muistaa antaa tiedostolle oikea polku ja nimi, sillä muuten tekstiä ei tallenneta oikeaan paikkaan. Käytä myös olemassa olevaa tiedostoa, jos haluat muokata sitä, sillä uuden tiedoston luominen samalla nimellä korvaa aiemman tiedoston. Voit myös käyttää "appendFile" -metodia, jos haluat lisätä tekstiä olemassa olevaan tiedostoon sen sijaan, että se korvaisi sen kokonaan.

## Katso myös
- [TypeScriptin viralliset ohjeet tekstin tallentamisesta tiedostoon](https://www.typescriptlang.org/docs/handbook/declarations.html)
- [Node.js "fs" -kirjaston dokumentaatio](https://nodejs.org/api/fs.html)