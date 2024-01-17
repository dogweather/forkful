---
title:                "Lähettämällä http-pyyntö"
html_title:           "TypeScript: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Kiitos HTTP-pyynnön: miksi ja MITÄ?
Lähettäessäsi HTTP-pyynnön, pyydät verkkosivulta tai palvelimelta tietoja. Tämä voi sisältää esimerkiksi hakemista, lähettämistä tai päivittämistä. Ohjelmoijat käyttävät tätä hyödyksi saadakseen esimerkiksi uusimmat tiedot palvelimelta tai tallentaakseen tietoja.

## Näin teet sen:
```TypeScript
import { HttpClient } from '@angular/common/http';

const url = 'https://example.com/api/users';

//luodaan uusi HTTP-asiakas
const http = new HttpClient();

//lähetetään GET-pyyntö ja haetaan vastaus
http.get(url).subscribe(response => {
    console.log(response);
}, error => {
    console.log(error);
})
```
Tässä esimerkissä käytetään Angularin HttpClient-kirjastoa lähettämään GET-pyyntö haluttuun URL-osoitteeseen. Vastaus saadaan Observable-muodossa, joka käsitellään sitten subscribe-metodilla.

## Syvempään perehtyminen:
HTTP-pyyntöjen käyttö on yleistynyt internetin kehittymisen myötä. Nykyään suurin osa verkkosivuista ja sovelluksista kommunikoi muiden palvelimien kanssa lähettämällä ja vastaanottamalla HTTP-pyyntöjä.

On myös muita tapoja lähettää ja vastaanottaa dataa kuin HTTP-pyynnöt, kuten esimerkiksi WebSockets, joka mahdollistaa reaaliaikaisen kommunikoinnin palvelimien kanssa. HTTP-pyyntöjen toteuttaminen on kuitenkin edelleen tärkeä ja yleinen tapa ohjelmoijien keskuudessa.

HTTP-pyyntöjen toteuttaminen TypeScriptillä on helppoa, sillä on olemassa valmiita kirjastoja, kuten Angularin HttpClient, joka tekee pyynnön lähettämisestä ja vastauksen käsittelystä yksinkertaista.

## Katso myös:
- [HttpClient Angularin dokumentaatiossa](https://angular.io/guide/http)
- [Express.js -framework Node.js-pohjaisten web-sovellusten kehittämiseen](https://expressjs.com/)
- [MDN WebSocketsin dokumentaatiossa](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API)