---
title:                "TypeScript: Tiedoston kirjoittaminen"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamista käytetään usein tiedon tallentamiseen pysyvästi ja helppoon jakamiseen muiden kanssa.

## Miten

Yksinkertaisin tapa kirjoittaa tekstitiedosto TypeScriptilla on käyttää `fs.writeFile()`-funktiota. Se ottaa ensimmäisenä parametrinaan halutun tiedoston nimen, toisena parametrinaan tekstisisältö ja kolmantena parametrina virheenkäsittelyfunktion.

```TypeScript
import * as fs from 'fs';

fs.writeFile('tekstitiedosto.txt', 'Tämä on tekstisisältöä.', (err) => {
  if (err) {
    console.log(err);
  } else {
    console.log('Tekstitiedosto on kirjoitettu onnistuneesti.');
  }
});
```

Tulostus koodin suorituksen jälkeen:

`Tekstitiedosto on kirjoitettu onnistuneesti.`

## Syventävä sukellus

Voit myös käyttää `fs.writeFileSync()`-funktiota, joka toimii samalla tavalla kuin `writeFile()`-funktio, mutta se ei ota vastaan virheenkäsittelyfunktiota. Tämän sijaan se heittää virheen suoraan, jos sellainen tapahtuu.

Voit myös lisätä tekstisisällön kirjoittamisen lisäksi muita toimintoja, kuten esimerkiksi tiedoston lukemisen ja sisällön lisäämisen olemassa olevaan tiedostoon. Kannattaa tutustua Node.js:n `fs`-moduulin dokumentaatioon löytääksesi lisää mahdollisia toimintoja.

## Katso myös

- Node.js vakiomoduuli `fs`: https://nodejs.org/api/fs.html
- `fs-extra`-moduuli: https://www.npmjs.com/package/fs-extra