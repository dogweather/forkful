---
title:                "Javascript: Perusso/Ohjelmointi: Lähetä http-pyyntö perusvarmennuksella"
simple_title:         "Perusso/Ohjelmointi: Lähetä http-pyyntö perusvarmennuksella"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi
HTTP-pyyntöjen lähettäminen perusautentikoinnilla on tärkeä taito web-kehityksessä, koska se mahdollistaa turvallisen tavan kommunikoida palvelimen ja käyttäjien välillä.

## Miten
```Javascript
// Luodaan uusi XMLHTTPRequest-olio
var xhr = new XMLHttpRequest();
// Määritetään pyynnön tyyppi ja URL
xhr.open('GET', 'https://example.com/api', true);
// Asetetaan otsikko käyttäjätunnukselle ja salasanalle HTTP-otsakkeessa
xhr.setRequestHeader('Authorization', 'Basic ' + btoa('käyttäjätunnus:salasana'));
// Lähetetään pyyntö ja käsitellään vastaus
xhr.send();
xhr.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
        console.log(xhr.responseText);
    }
};
```

Ylläolevassa koodiesimerkissä luodaan uusi XMLHTTPRequest-olio ja lähetetään GET-pyyntö palvelimelle. Pyyntöön lisätään HTTP-otsikossa perusautentikointi, jossa käyttäjätunnus ja salasana on encodettu base64-muotoon. Pyyntöön vastauksen saapuessa, tarkistetaan sen tila ja status, ja jos vastaus on onnistunut, tulostetaan vastauksen sisältö konsoliin.

## Syvempi katsaus
Perusautentikointi on yleinen tapa suojata web-sovelluksia käyttäjien lähettämiltä pyynnöiltä. Sitä käytetään usein yhdessä muiden turvallisuusmenetelmien, kuten SSL-sertifikaattien, kanssa. Perusautentikoinnin toimintaperiaate on yksinkertainen: käyttäjän tulee lähettää pyyntöön mukanaan käyttäjätunnus ja salasana, jotka on encodettu base64-muotoon. Palvelin tarkistaa sitten näiden tietojen oikeellisuuden ja vastaa sen mukaan.

HTTP-pyyntöjen lähettäminen perusautentikoinnilla voi myös helpottaa monimutkaisempien käyttäjätunnusten ja salasanojen käsittelyä. Se tarjoaa myös helpon ja nopean tavan suojata erilaisia web-sovelluksia, kuten REST APIjaä tai Ajax-kutsuja.

## Katso myös
* [XMLHttpRequest - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
* [Base64 Encoding - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/WindowBase64/Base64_encoding_and_decoding)
* [Basic Access Authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)