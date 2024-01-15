---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "TypeScript: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi
Kehittäjät voivat lähettää HTTP-pyyntöjä kommunikoidakseen web-palvelimien kanssa ja hakeakseen tai päivittääkseen tietoja. Tämä on tärkeä osa web-kehitystä ja voi auttaa luomaan yhteyspisteitä muiden sovellusten kanssa.

## Miten tehdä se
Käyttämällä TypeScriptiä ja sen sisäänrakennettuja kirjastoja, kuten Axios, kehittäjät voivat lähettää HTTP-pyyntöjä helposti ja tehokkaasti. Tässä on yksinkertainen esimerkki GET-pyynnön lähettämiseksi Axios-kirjastolla:

```TypeScript
import axios from 'axios';

axios.get('https://example.com/api/users')
  .then((response) => {
    console.log(response.data);
  })
  .catch((error) => {
    console.log(error);
  });
```

Koodi lähettää GET-pyynnön osoitteeseen "https://example.com/api/users" ja tulostaa vastauksen konsoliin. Koodissa käytämme myös promiseja käsittelemään vastausta ja mahdollisia virheitä.

## Syväsukellus
HTTP-pyynnöt ovat tärkeä osa RESTful-arkkitehtuuria ja ovat tärkeä tapa siirtää tietoja eri sovellusten välillä. TypeScript tarjoaa kehittäjille helpon tavan lähettää erilaisia HTTP-pyyntöjä, kuten GET, POST ja PUT. Kehittäjät voivat myös käyttää muita kirjastoja, kuten SuperAgent ja Fetch, lähettääkseen HTTP-pyyntöjä ja käsitellä vastauksia.

## Katso myös
- [TypeScriptin virallinen verkkosivusto](https://www.typescriptlang.org/)
- [Axios-kirjaston dokumentaatio](https://github.com/axios/axios)
- [RESTful-sovellusten suunnittelu](https://restfulapi.net/)