---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "TypeScript: HTTP-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Mitä & Miksi?:
Lähettämällä HTTP-pyyntö perusautentikointiin voit suojata tiedonsiirtoa ja varmistaa, että vain oikeutetut käyttäjät voivat käyttää palveluasi. Tämä on tärkeää etenkin silloin, kun käsitellään arkaluonteisia tietoja, kuten henkilökohtaisia ​​tietoja. Ohjelmoijat käyttävät perusautentikointia tietoturvan lisäämiseksi sovelluksiinsa.

Kuinka tehdä:
 Esimerkki koodilla TypeScript käyttäen Axios-kirjastoa:
```
const axios = require('axios');

axios.get('https://example.com/api/', {
  auth: {
    username: 'käyttäjänimi',
    password: 'salasana'
  }
})
.then(response => {
  console.log(response.data);
})
.catch(error => {
  console.log(error);
});
```
Tämä koodi lähettää GET-pyynnön "https://example.com/api/" osoitteeseen perusautentikoinnilla, käyttäen käyttäjänimeä ja salasanaa. Pyynnön vastauksena palautetaan saatu tieto, tai virheilmoitus, jos jokin menee pieleen.

Syväsukellus:
Perusautentikointi on yksi vanhimmista ja yksinkertaisimmista tapoista varmistaa tietoturva HTTP-pyyntöjen lähettämisessä. Siinä käytetään Base64-koodausta salasanan ja käyttäjänimen yhdistelmään, joka lähetetään Authorization-otsakkeessa pyynnön mukana. On kuitenkin tärkeää huomata, että perusautentikointi ei tarjoa vahvaa suojaa ja on altis tietojen hakkeroinnille. On suositeltavaa käyttää muita autentikointi menetelmiä, kuten Digest tai OAuth.

Katso myös:
- https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html