---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
date:                  2024-01-20T18:02:46.712294-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"

category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lähettäessäsi HTTP-pyyntöä perusautentikoinnilla lisäät käyttäjätunnuksen ja salasanan pyyntöösi. Ohjelmoijat tekevät tämän varmistaakseen käyttöoikeudet ennen tietojen vastaanottamista tai lähettämistä.

## Kuinka:
```TypeScript
import axios from 'axios';

// Käytäjätunnuksesi ja salasanasi
const username: string = 'kayttaja';
const password: string = 'salasana';

// Perusautentikaation tokenin luonti
const basicAuth: string = 'Basic ' + Buffer.from(username + ':' + password).toString('base64');

// HTTP-pyyntö
axios.get('https://api.esimerkki.fi/data', {
  headers: { Authorization: basicAuth }
})
.then(response => {
  console.log(response.data);
})
.catch(error => {
  console.error('Virhe pyynnön aikana:', error);
});
```

Sample output:
```
{ tiedot: "Tässä on vastauksesi data." }
```

## Syväsukellus
Perusautentikaatio HTTP:ssä on vanha menetelmä, jossa käyttäjätunnus ja salasana lähetetään Base64-koodattuina. Turvallisempi vaihtoehto moderniin API-viestintään on OAuth tai JWT (JSON Web Tokens), jotka tarjoavat vahvemman salauksen ja lisäominaisuuksia. Perusautentikaatiota käytettäessä transmissiosuojaus on olennaista (käytä HTTPS), jotta tietojen urkinta estetään.

Toteutuksessa on tärkeää käsitellä myös virhevastauksia HTTP-pyynnöissä. Esimerkkikoodissa käytetään axios-kirjastoa, joka on suosittu vaihtoehto HTTP-pyyntöjen tekemiseen, ja se tukee lupauksiin (promises) perustuvaa asynkronista käsittelyä virheiden hallintaan.

## Katso Myös
- MDN Web Docs, Basic authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme
- Axios GitHub repository: https://github.com/axios/axios
- OAuth 2.0 authorization framework: https://oauth.net/2/
- JWT (JSON Web Tokens): https://jwt.io/
