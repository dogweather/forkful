---
title:                "TypeScript: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

### Miksi

Monet ihmiset haluavat ladata verkkosivuja omalle tietokoneelleen eri syistä, kuten offline-käyttöä varten, tietojen varmuuskopiointia varten tai vaikkapa webskrapingin tarpeisiin. TypeScript tarjoaa helpon ja tehokkaan tavan ladata verkkosivuja suoraan koodista.

### Miten tehdä

Verkkosivun lataaminen TypeScriptillä on yksinkertaista. Ensinnäkin, tuodaan sisään `axios` -kirjasto, joka auttaa meitä tekemään HTTP-pyyntöjä.

```
TypeScript
import axios from 'axios';
```

Seuraavaksi luodaan funktio, joka ottaa argumenttina verkkosivun URL-osoitteen.

```
TypeScript
function lataaVerkkosivu(url) {
    // koodi tänne
}
```

Käytämme `axios` -kirjaston `get` -metodia lähettääksemme GET-pyynnön verkkosivulle ja tallentamaan vastauksen muuttujaan

```
TypeScript
async function lataaVerkkosivu(url) {
    const vastaus = await axios.get(url);
}
```

Lopuksi voimme tulostaa vastauksen konsoliin ja nähdä, mitä tiedot saimme verkkosivulta.

```
TypeScript
async function lataaVerkkosivu(url) {
    const vastaus = await axios.get(url);
    console.log(vastaus.data);
}
```

Nyt voimme kutsua funktiota antamalla sille halutun verkkosivun URL-osoitteen.

```
TypeScript
lataaVerkkosivu('https://www.example.com');
```

Tämän koodin suorittaminen tulostaisi konsoliin verkkosivun HTML-koodin.

### Syvällisempi sukellus

Voit myös ladata muita tiedostotyyppejä verkkosivuilta, kuten kuvia tai dokumentteja, käyttämällä `axios` -kirjastoa. Voit myös käyttää muita kirjastoja, kuten `puppeteer`, jos haluat jopa suorittaa JavaScript-koodia verkkosivulla.

### Katso myös

- [Axios - dokumentaatio](https://www.npmjs.com/package/axios)
- [Puppeteer - dokumentaatio](https://pptr.dev/)