---
title:                "Tietokoneohjelmointi: Verkkosivun lataaminen"
html_title:           "TypeScript: Tietokoneohjelmointi: Verkkosivun lataaminen"
simple_title:         "Tietokoneohjelmointi: Verkkosivun lataaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Web-sivun lataaminen voi olla tärkeä osa verkkokehitystä, sillä se mahdollistaa sivuston sisällön tallentamisen paikallisesti ja sen tietojen käytön muilla ohjelmilla.

## Kuinka

```TypeScript
import axios from 'axios';

// Ladataan web-sivu käyttäen axios-kirjastoa
axios.get('https://www.esimerkkisivu.fi').then(response => {
    // Tulostetaan vastauksen data
    console.log(response.data);
});
```

Tällä yksinkertaisella koodilla voidaan ladata haluttu web-sivu ja saada talteen sen sisältö. Käyttämällä HTTP-pyyntökirjastoa, kuten axios, lataaminen käy helposti ja luotettavasti. 

## Syväsukellus

Web-sivujen lataamisella on monia käyttötarkoituksia, kuten sivuston sisällön muokkaaminen ja kääntäminen eri kielille, tiedon kerääminen tai verkkorobotin luominen. Lataaminen voidaan tehdä myös taustaprosessina ohjelman suorituksen aikana.

## Katso myös

- [axios-kirjaston dokumentaatio](https://github.com/axios/axios)
- [Web-sivun lataaminen käyttämällä JavaScriptiä](https://www.digitalocean.com/community/tutorials/how-to-use-node-js-request-and-cheerio-to-set-up-simple-web-scraping)
- [Web-sivujen lataaminen ja käyttö paikallisesti](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Client-side_web_APIs/Fetching_data)