---
title:                "TypeScript: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Tekstinlukeminen on yksinkertainen ja tehokas tapa käsitellä suuria määriä tietoa. Se voi auttaa sinua tunnistamaan kaavoja, etsimään tiettyjä tietoja ja tekemään yksinkertaisia muokkauksia suurelle määrälle tiedostoja.

## Miten

```TypeScript
// Tuodaan sisään tarvittavat moduulit
import fs from 'fs';

// Luodaan funktio tiedoston lukemiseksi
function lueTeksti(tiedostonimi: string) {
  
  // Luodaan tietopuskuri ja muunnetaan sen sisältö tekstimuotoon
  const tietopuskuri: Buffer = fs.readFileSync(tiedostonimi);
  const teksti: string = tietopuskuri.toString();

  // Tulostetaan teksti konsoliin
  console.log(teksti);
}

// Kutsutaan funktiota ja annetaan tiedoston nimi parametrina
lueTeksti("tiedosto.txt");

```

Esimerkkitiedoston "tiedosto.txt" sisältö:

```
Tämä on esimerkki teksti tiedostosta.
Tässä on useita rivejä erilaisilla tietoaineistoilla.
Voit lukea, muokata ja analysoida näitä tietoja käyttäen TypeScriptiä.
```

Tulostuu konsoliin:

```
Tämä on esimerkki teksti tiedostosta.
Tässä on useita rivejä erilaisilla tietoaineistoilla.
Voit lukea, muokata ja analysoida näitä tietoja käyttäen TypeScriptiä.
```

## Syvällinen tutkimus

Tekstin lukeminen TypeScriptillä käyttäen "fs" (file system) moduulia on helppoa ja nopeaa. Voit käyttää erilaisia metodeja muuttaaksesi tiedostojen sisältöä ja suorittaa erilaisia käsittelyä kyseiselle tiedostolle. Voit myös käyttää muita muotoilutyökaluja, kuten Regular Expression, analysoimaan ja manipuloimaan tekstiä.

## Katso myös
- [TypeScriptin virallinen sivusto](https://www.typescriptlang.org/)
- [fs moduulin dokumentaatio](https://nodejs.org/api/fs.html)