---
title:                "Javascript: Http-pyyntöjen lähettäminen"
simple_title:         "Http-pyyntöjen lähettäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on olennainen osa verkkosivujen ja sovellusten kehittämistä. Se mahdollistaa tietojen hakemisen ja lähettämisen palvelimelle ja takaisin käyttäjälle. Ilman HTTP-pyyntöjä sivut jäisivät staattisiksi ja rajoittuneiksi.

## Miten

HTTP-pyyntöjen lähettäminen tapahtuu yksinkertaisesti käyttämällä ``fetch``-komentoa ja antamalla tarvittavat tiedot, kuten URL-osoite ja pyynnön tyyppi. Esimerkiksi, jos haluaisimme lähettää GET-pyynnön, voisimme käyttää seuraavaa koodia:

```Javascript
fetch('https://example.com/users')
  .then(response => response.json())
  .then(data => console.log(data));
```

Tämä koodi lähettää pyynnön osoitteeseen "https://example.com/users" ja odottaa vastausta. Kun vastaus saapuu, se muunnetaan JSON-muotoon ja tulostetaan konsoliin. On tärkeää varmistaa, että pyyntöä seuraava ``then``-komento käsittelee vastauksen oikeassa muodossa, jotta voimme hyödyntää palvelimen lähettämiä tietoja.

## Syvällisempi sukellus

HTTP-pyyntöjen lähettäminen sisältää useita erilaisia tietoja ja vaihtoehtoja, kuten otsikot ja parametrit. Näitä voidaan käyttää lisäämään tietoturvatasoa ja hallitsemaan pyynnön tarkoitusta ja sisältöä. Esimerkiksi, jos haluamme lähettää POST-pyynnön, voimme lisätä parametreja, jotka sisältävät lähettämämme tiedot:

```Javascript
fetch('https://example.com/users', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({
    name: 'John',
    age: 30
  })
})
.then(response => response.json())
.then(data => console.log(data));
```

Tässä käytämme parametreja määrittämään pyynnön tyyppi, otsikot ja lähettävät tiedot JSON-muodossa.

## Katso myös

- [MDN - Fetch API](https://developer.mozilla.org/fi/docs/Web/API/Fetch_API)
- [W3Schools - HTTP Request methods](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [Stack Overflow - Fetch vs XMLHttpRequest](https://stackoverflow.com/questions/35901481/what-is-the-difference-between-fetch-and-xmlhttprequest)