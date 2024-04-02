---
date: 2024-01-20 18:02:46.712294-07:00
description: "L\xE4hett\xE4ess\xE4si HTTP-pyynt\xF6\xE4 perusautentikoinnilla lis\xE4\
  \xE4t k\xE4ytt\xE4j\xE4tunnuksen ja salasanan pyynt\xF6\xF6si. Ohjelmoijat tekev\xE4\
  t t\xE4m\xE4n varmistaakseen k\xE4ytt\xF6oikeudet\u2026"
lastmod: '2024-03-13T22:44:56.315133-06:00'
model: gpt-4-1106-preview
summary: "L\xE4hett\xE4ess\xE4si HTTP-pyynt\xF6\xE4 perusautentikoinnilla lis\xE4\xE4\
  t k\xE4ytt\xE4j\xE4tunnuksen ja salasanan pyynt\xF6\xF6si. Ohjelmoijat tekev\xE4\
  t t\xE4m\xE4n varmistaakseen k\xE4ytt\xF6oikeudet\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
weight: 45
---

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
