---
title:                "Perusautentikoinnin lähettäminen http-pyyntönä"
html_title:           "TypeScript: Perusautentikoinnin lähettäminen http-pyyntönä"
simple_title:         "Perusautentikoinnin lähettäminen http-pyyntönä"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyynnön lähettäminen perusautentikoinnilla on tärkeää, kun halutaan suojata tietyn verkkopalvelun resursseja. Näin varmistetaan, että vain oikeutetut käyttäjät pääsevät käsiksi tiettyihin resursseihin ja estetään luvattomat käyttäjät aiheuttamasta vahinkoa.

## Miten tehdä se

```TypeScript
import axios from 'axios';

const username = 'käyttäjänimi';
const password = 'salasana';
const url = 'https://esimerkki.com/api/resource';

axios.get(url, {
  auth: {
    username: username,
    password: password
  }
}).then((response) => {
  console.log(response.data);
}).catch((error) => {
  console.log(error);
});
```

Kun lähetetään HTTP-pyyntö perusautentikoinnilla, käyttäjänimi ja salasana tulee lisätä pyynnön otsikkoon Base64-muodossa. Tämä voidaan tehdä myös manuaalisesti, mutta esimerkiksi Axios-kirjaston avulla tämä hoituu helposti asettamalla `auth`-parametri pyynnön asetuksissa.

## Syvemmälle

Perusautentikoinnissa käytetään HTTP-pyyntöjen otsikkossa `Authorization`-kenttää, joka sisältää käyttäjänimen ja salasanan Base64-koodattuna. Näin käyttäjänimi ja salasana eivät näy selväkielisenä vaan ovat salattuna.

On tärkeää huomata, että perusautentikointi ei ole kaikkein turvallisin tapa suojata resursseja, sillä Base64-koodaus on helppo purkaa. Suositeltavampi vaihtoehto olisi käyttää esimerkiksi JWT-tokensseja.

## Katso myös

- [Axios dokumentaatio](https://axios-http.com/)
- [Base64-koodaus selitys (englanniksi)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [JWT-tokenten käyttö HTTP-pyynnöissä](https://jwt.io/introduction/)