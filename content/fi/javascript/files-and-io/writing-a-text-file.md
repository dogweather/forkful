---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:30.211979-07:00
description: "Tekstitiedoston kirjoittaminen JavaScriptill\xE4 liittyy usein datan\
  \ luomiseen ja tallentamiseen yksinkertaisessa, luettavassa muodossa lokitusta,\u2026"
lastmod: '2024-03-13T22:44:56.968191-06:00'
model: gpt-4-0125-preview
summary: "Tekstitiedoston kirjoittaminen JavaScriptill\xE4 liittyy usein datan luomiseen\
  \ ja tallentamiseen yksinkertaisessa, luettavassa muodossa lokitusta, k\xE4ytt\xE4\
  j\xE4sy\xF6tteen viemist\xE4 tai konfiguraatiotarkoituksia varten."
title: Tekstitiedoston kirjoittaminen
weight: 24
---

## Kuinka:
Node.js-ympäristössä voit käyttää sisäänrakennettua `fs` (File System) moduulia tekstitiedostojen kirjoittamiseen. Tämä esimerkki demonstroi tekstin kirjoittamista tiedostoon asynkronisesti:

```javascript
const fs = require('fs');

const data = 'Hello, World! This is text to be written into a file.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Tiedosto on kirjoitettu.');
});
```

Esimerkkituloste:
```
Tiedosto on kirjoitettu.
```

Synkronista tiedoston kirjoittamista varten käytä `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('Tiedosto on kirjoitettu.');
} catch (error) {
  console.error('Tiedoston kirjoittamisvirhe:', error);
}
```

Nykyisissä verkkoselaimissa File System Access API tuo mahdollisuuden lukea ja kirjoittaa tiedostoja. Sen käyttö on kuitenkin käyttäjän lupien alaista. Tässä on esimerkki, kuinka luoda ja kirjoittaa tiedostoon:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hello, World! This is browser text file writing.');
  await writable.close();
}
```

Monimutkaisemmissa skenaarioissa tai suurten tiedostojen kanssa työskennellessä saatat valita kolmannen osapuolen kirjastoja, kuten `FileSaver.js` selaimille:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hello, World! This is text from FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

Muista, että tiedostojen kirjoittaminen asiakaspuolella (selaimissa) on rajoitettua turvallisuussyistä, ja kaikki toiminnot, jotka edellyttävät tallentamista käyttäjän paikalliselle levylle, vaativat yleensä heidän nimenomaisen luvansa.
