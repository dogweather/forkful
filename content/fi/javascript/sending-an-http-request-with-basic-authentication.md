---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyynnön lähettäminen perusautentikoinnilla on menetelmä, jolla varmennetaan käyttäjän henkilöllisyys verkossa. Ohjelmoijat käyttävät sitä tietoturvan varmistamiseksi ja luvattoman pääsyn estämiseksi.

## Miten:

Javascriptissa voidaan lähettää HTTP-pyyntö perusautentikoinnilla käyttämällä `fetch` funktiota. Tässä on esimerkkikoodi:

```Javascript
const username = 'kayttajatunnus';
const password = 'salasana';

let headers = new Headers();

headers.set('Authorization', 'Basic ' + btoa(username + ":" + password));

fetch('https://example.com',{
    method:'GET',
    headers: headers
})
.then(response => response.json())
.then(data => console.log(data));
```

Jos pyyntö onnistui, tulostetaan palvelimelta palautettu data konsoliin.

## Sukellus syvyyksiin

Perusautentikoinnin käyttö HTTP-pyynnöissä on käytäntö, joka juontaa juurensa www-verkon alkuaikoihin. Se on yksinkertainen, mutta tehokas tapa varmistaa, että pyynnön lähettäjällä on oikeat käyttöoikeudet. Vaikka nykymaailmassa on käytössä monimutkaisempia ja turvallisempia autentikointimenetelmiä, perusautentikointi on edelleen yleinen nopean ja yksinkertaisen autentikoinnin vuoksi.

JavaScriptin `fetch`-funktio on vain yksi tapa lähettää HTTP-pyyntöjä. Axios on toinen suosittu työkalu, jolla voit tehdä saman. Kuten `fetch`, Axiosilla voit lähettää pyyntöjä sekä selaimesta että Node.js-sovelluksesta.

Lähettäessäsi HTTP-pyynnön perusautentikoinnilla, käyttäjänimesi ja salasanasi koodataan Base64-muotoon. Tätä autentikointitietoa ei kuitenkaan salata, joten tietoturvasyistä perusautentikointia ei tule käyttää ilman HTTPS-yhteyttä.

## Katso myös:

- Fetch API: [https://developer.mozilla.org/fi/docs/Web/API/Fetch_API](https://developer.mozilla.org/fi/docs/Web/API/Fetch_API)
- Axios: [https://axios-http.com/](https://axios-http.com/)