---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lataaminen tarkoittaa verkkosivujen tietojen hakemista palvelimelta. Ohjelmoijat tekevät näin, kun heidän täytyy saada pääsy ja käsitellä verkkosivun sisältöä, ajaa skriptejä tai tallentaa tietoja myöhempää käyttöä varten.

## Miten tehdään:

Käytämme axios-kirjastoa TypeScriptissä verkkosivun lataamiseksi. Asennetaan se ensin.

```npm
npm install axios
```

Koodinäyte:

```TypeScript
import axios from 'axios';

const downloadPage = async (url: string): Promise<void> => {
  try {
    const response = await axios.get(url);
    console.log(response.data);
  } catch (error) {
    console.error(error);
  }
};

downloadPage('https://www.example.com');
```

## Syväsukellus:

Teknologia kehittyy jatkuvasti, ja verkkosivujen lataaminen ei ole poikkeus. Historiallisesti tämä tehtiin usein puhelinlinja-viritetyillä modeemeilla, mutta nykyään prosessi on paljon nopeampi ja tehokkaampi.

On myös vaihtoehtoja axios-kirjastolle, kuten fetch API ja request Promise, jotka ovat käyttökelpoisia jotkut skenaariot. Suosittelemme tutkimista ja valitsemaan sen, mikä sopii parhaiten projektisi vaatimuksiin.

Axios-kirjasto käsittelee HTTP-pyynnöt ja vastaukset. Käytämme tässä esimerkissä GET-menetelmää, joka on yleisin tapa ladata verkkosivuja. Axios palauttaa lupauksen, joka ratkaistaan, kun vastaus saapuu.

## Katso myös:

1. [Axios GitHub](https://github.com/axios/axios) - Täydellinen axios-kirjaston dokumentaatio
2. [Fetch API](https://developer.mozilla.org/fi/docs/Web/API/Fetch_API) - Kätevä tapa tehdä verkkopohjaisia ​​pyyntöjä
4. [HTTP-viestit](https://developer.mozilla.org/fi/docs/Web/HTTP/Messages) - Selitys HTTP-pyynnöistä ja vastauksista
5. [Lupaukset JavaScriptissä](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Using_promises) - Perusteellisempi katsaus lupauksiin.