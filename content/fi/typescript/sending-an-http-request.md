---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen on, kun ohjelma luo ja lähettää HTTP-muotoisen viestin palvelimelle. Ohjelmoijat tekevät tämän tiedon saamiseksi palvelimelta tai tietojen lähettämiseksi palvelimelle. 

## Kuinka:
Näin lähetetään HTTP-pyyntö TypeScriptillä axios-kirjaston avulla.
Lisää axios-projektiisi seuraavasti:

```TypeScript
npm install axios
```

Tämän jälkeen voit lähettää HTTP-pyyntöjä seuraavasti:

```TypeScript
import axios from 'axios';

axios.get('https://api.example.com')
    .then(response => {
        console.log(response.data);
    })
    .catch(error => {
        console.error(error);
    });
```
Tämä ohjelma tulostaa palvelimen vastauksen konsoliin.

Kohta "catch" käsittelee virhetapaukset. 

## Sukellus syvemmälle
HTTP-pyynnöt ovat olennainen osa web-ohjelmointia. Ne otettiin käyttöön ensimmäisen kerran 1990-luvun alussa, kun web alkoi kehittyä. 

On olemassa monia tapoja lähettää HTTP-pyyntöjä. Axios on yksi suosituimmista kirjastoista JavaScript/TypeScript -ympäristössä http-pyyntöjen tekemiseen, mutta voit halutessasi käyttää myös muita kirjastoja, kuten fetch tai superagent. 

Axios-kirjasto tekee useita asioita http-pyyntöprosessissa automaattisesti, jotta sen käyttäjien ei tarvitse. Axios käsittelee esimerkiksi virheiden tarkistamisen, pyynnön muotoilun ja vastauksen lukemisen automaattisesti. 

## Katso myös: 
Lue lisää axios-kirjastosta ja HTTP-pyynnoistä seuraavista lähteistä:

1. Axios-kirjaston virallinen dokumentaatio: [axios](https://axios-http.com/docs/intro)
2. Erittäin yksityiskohtainen johdatus HTTP-protokollaan: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP)
3. Google Developersin koostamaa tietoa HTTP-pyynnöistä ja -vasteista: [Google Developers](https://developers.google.com/web/fundamentals/performance/http2/)