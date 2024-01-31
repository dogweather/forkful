---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Kirjoittaa tekstitiedosto tarkoittaa tiedon tallentamista tekstimuodossa tiedostoon. Koodarit tekevät tätä datan pysyvään säilytykseen, asetusten tallennukseen tai lokitiedostojen luomiseen.

## How to:

Node.js:lla tallennetaan tiedosto `fs`-moduulin avulla:

```Javascript
const fs = require('fs');

let data = "Terve maailma!";

fs.writeFile('tervetuloa.txt', data, (err) => {
    if (err) throw err;
    console.log('Tiedosto tallennettu!');
});
```

Selaimessa käytä `Blob`-objektia ja tallenna tiedosto `a`-elementin `href`-ominaisuuden kautta:

```Javascript
let data = "Terve selainmaailma!";
let blob = new Blob([data], { type: 'text/plain' });

let a = document.createElement('a');
a.download = 'terveiset.txt';
a.href = window.URL.createObjectURL(blob);
a.style.display = 'none';
document.body.appendChild(a);
a.click();
document.body.removeChild(a);
```

## Deep Dive

Kirjoittaminen tekstitiedostoon on ollut tärkeä osa ohjelmointia alusta alkaen. Erityisesti palvelimella Node.js:n `fs`-moduuli on standardityökalu tähän, kun taas selaimessa tiedoston kirjoitusominaisuudet ovat rajatumpia turvallisuussyistä. Vaihtoehtoisesti voit käyttää tietokantoja tai pilvipalveluita pysyvään datan tallennukseen. Implementaatiotiedot vaihtelevat alustoittain, kuten tiedostonkäsittelyssä käytettävät käyttöoikeudet tai prosessit.

## See Also

- Node.js FileSystem Documentation: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- MDN Web Docs Blob-käyttö: [https://developer.mozilla.org/en-US/docs/Web/API/Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
- HTML Living Standard for a.download: [https://html.spec.whatwg.org/#the-a-element](https://html.spec.whatwg.org/#the-a-element)
