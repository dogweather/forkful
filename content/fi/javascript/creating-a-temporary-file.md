---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---

## Mitä & Miksi?

Tilapäisten tiedostojen luonti tarkoittaa väliaikaisten tiedostojen luomista järjestelmän muistiin. Tämä on kätevää esimerkiksi datan tallentamiseen, jota voidaan käsitellä ja jota ei tarvitse säilyttää järjestelmässä pitkällä aikavälillä.

## Kuinka:

Esimerkissä käytetään `tmp-promise` kirjastoa, jonka avulla voidaan luoda ja hallinnoida tilapäisiä tiedostoja. Asenna kirjasto komennolla `npm install --save tmp-promise`.

```javascript
const tmp = require('tmp-promise');

tmp.file()
  .then((tempFile) => {
    console.log(tempFile.path); // Tulostaa tilapäisen tiedoston polun
  })
  .catch((err) => console.error(err));
```

Koodinpätkä luo tilapäisen tiedoston ja tulostaa tiedoston polun.

## Syvällisempi tarkastelu:

Tilapäisten tiedostojen käyttöön liittyvät käytännöt ovat vaihdelleet eri ohjelmointikielien ja -ympäristöjen kesken. JavaScriptin osalta on useita vaihtoehtoisia kirjastoja, kuten `tmp`, `temp` ja `tmp-promise`. Edellä mainitussa esimerkissä käytimme `tmp-promise`-kirjastoa, joka tukee lupaukset (Promises) ja tekee koodista selkeämpää.

Tilapäisten tiedostojen luominen tapahtuu yleensä järjestelmän muistissa tai väliaikaissäilytykseen tarkoitetussa kansiossa. Tämän toteutus riippuu käytetystä käyttöjärjestelmästä ja JavaScript-ympäristöstä.

## Katso myös:

Lisätietoa löytyy seuraavista lähteistä:

- `tmp-promise` kirjaston dokumentaatio: [https://github.com/benjamingr/tmp-promise](https://github.com/benjamingr/tmp-promise)


Koodiesimerkki perustuu lähteestä: [https://www.npmjs.com/package/tmp-promise](https://www.npmjs.com/package/tmp-promise)

---