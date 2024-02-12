---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
aliases:
- /fi/javascript/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:01:59.400463-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Perustiedot: HTTP-pyyntö basic-autentikaatiolla tarkoittaa käyttäjänimen ja salasanan sisällyttämistä HTTP-pyyntöön tunnistautumista varten. Ohjelmoijat käyttävät tätä tapaa turvatakseen resurssien pääsy vain valtuutetuille käyttäjille.

## How to:
Javaskriptissä basic-autentikaatio HTTP-pyynnössä onnistuu `fetch`-funktiolla tai kirjastojen (esim. Axios) avulla. Tässä esimerkki `fetch`-käytöstä:

```javascript
const username = 'kayttaja';
const password = 'salasana';
const base64Credentials = btoa(`${username}:${password}`);

fetch('https://example.com/data', {
  method: 'GET',
  headers: {
    'Authorization': `Basic ${base64Credentials}`,
  },
})
.then(response => response.json())
.then(data => console.log(data))
.catch(error => console.error('Virhe:', error));
```

Esimerkin tulostus riippuu palvelimelta saatavasta datasta.

## Deep Dive:
Basic-autentikaatio liittyy alkuaikojen webin yksinkertaisiin tunnistautumismekanismeihin. Tänään sen käyttöä pidetään yksinkertaisena, mutta usein riittämättömänä suojauksen kannalta, sillä tunnukset lähetetään selkokielellä (base64-enkoodattuna) ilman salausta. Ohjelmoijat siirtyvät yhä useammin vahvempiin menetelmiin, kuten OAuth2:een tai JWT-tokensiin. 

Kun käytät basic-autentikaatiota, varmistu aina HTTPS-yhteydestä, joka suojaa tietoja salakatselulta. Suorituskyvyn kannalta basic-autentikaation käsittely palvelimella on nopeaa, mutta lisää kuormaa toistuvilla pyynnöillä, koska tunnukset on aina lähetettävä uudelleen.

## See Also:
- MDN Web Docs, Basic authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- OWASP, Basic Authentication: https://owasp.org/www-community/controls/Basic_Authentication
- Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Axios library: https://axios-http.com/
