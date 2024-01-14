---
title:                "TypeScript: Tarkista, onko hakemisto olemassa."
simple_title:         "Tarkista, onko hakemisto olemassa."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko hakemisto olemassa?

On monia tilanteita, joissa ohjelmoijan on tarkistettava, onko tietyssä sijainnissa olemassa olevaa hakemistoa tai tiedostoa. Tämä voi olla tarpeen ohjelman suorituksen aikana, jotta voidaan varmistaa tietyn toiminnon oikeellisuus tai tarvittavan tiedoston saatavuus. Se voi myös auttaa välttämään virheitä, kuten yrityksiä käsitellä ei-olemassa olevaa hakemistoa tai tiedostoa.

## Miten tarkistaa, onko hakemisto olemassa?

Tarkistaaksesi, onko hakemisto olemassa TypeScriptillä, voit käyttää Node.js:n *fs* -moduulia. Tämä moduuli sisältää *stat* -metodin, joka voidaan käyttää tarkistamaan, onko tiedostopolku olemassa. Seuraavassa esimerkissä tarkistetaan, onko hakemisto *testi* olemassa ja kaivetaan konsoliin vastaava viesti:

```TypeScript
import fs from 'fs';

const hakemistopolku = './testi';

fs.stat(hakemistopolku, (err, stats) => {
  if (err) {
    // Hakemistoa ei löytynyt, tulostetaan virheviesti
    console.log('Hakemistoa ei löytynyt!');
  } else {
    // Hakemisto löytyi, tulostetaan viesti
    console.log('Hakemisto löytyi!');
  }
})
```

Tulostus olisi seuraavanlainen:

```console
Hakemisto löytyi!
```

On myös huomattava, että *stat* -metodi tuottaa *FileStats* -objektin, jossa on tietoa hakemiston tai tiedoston koosta ja muutoksista. Tämän tiedon avulla voit tehdä edistyneempiä tarkistuksia tarpeidesi mukaan.

## Syvällisempi tarkastelu

Harkitse, että haluat tarkistaa, onko hakemistossa olemassa olevaa tiedostoa, ennen kuin luot sen uudestaan. Tämä voi olla tapaus, jossa haluat varmistaa, ettei käyttäjä menetä tärkeitä tietoja tekemällä samannimisen tiedoston uudelleen. Voit tehdä tämän käyttämällä *stat* -metodia ja tarkistamalla, onko tiedostolle annettu päivämäärä uudempi kuin tietyllä aikavälillä. Seuraava esimerkki havainnollistaa tätä ajatusta:

```TypeScript
import fs from 'fs';

const tiedostonimi = 'tiedosto.txt';

fs.stat(tiedostonimi, (err, stats) => {
  if (err) {
    // Tiedostoa ei löytynyt, luodaan uusi
    fs.writeFile(tiedostonimi, 'Tärkeitä tietoja!', (err) => {
      if (err) throw err;
      console.log('Uusi tiedosto luotu!');
    });
  } else {
    // Tiedosto löytyi, tarkistetaan päivämäärä
    const uusinPaivays = Date.now() - 86400000; // Tarkistetaan viimeisen 24h aikana
    if (stats.mtimeMs > uusinPaivays) {
      console.log('Tiedosto on luotu viimeisen päivän aikana, joten se on uudempi. Ei tarvetta uudelleenluomiseen.');
    } else {
      console.log('Tiedosto on luotu yli päivä sitten, joten luodaan uusi versio varmuuden vuoksi.');
      fs.writeFile(tiedostonimi, 'Tärkeitä tietoja!', (err) => {
        if (err) throw err;
        console.log('Uusi tiedosto luot