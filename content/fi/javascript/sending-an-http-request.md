---
title:                "Lähettämällä http-pyyntö"
html_title:           "Javascript: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyynnön lähettäminen on tapa kommunikoida verkon kautta tietokoneiden välillä. Tietokoneet lähettävät ja vastaanottavat näitä pyyntöjä, jotta ne voivat vaihtaa tietoa ja suorittaa tehtäviä toistensa kanssa. Koodaajat käyttävät HTTP-pyyntöjä esimerkiksi hakeakseen tietoja toisilta verkkosivustoilta tai lähettääkseen tietoa palvelimille.

## Miten:

```JavaScript
fetch('https://www.example.com/api/data') //luo uuden HTTP-pyynnön
.then(response => response.json()) //odottaa vastausta ja muuttaa sen JSON-muotoon
.then(data => { //tulostaa palvelimelta saadun datan
  console.log(data);
})
.catch(err => {
  console.log(err); //jos pyyntö epäonnistuu, tulostaa virheilmoituksen
});
```
Tämä koodiesimerkki osoittaa, miten HTTP-pyyntö lähetetään käyttäen JavaScriptin fetch-funktiota. Vastaanotettu data muutetaan JSON-muotoon ja tulostetaan konsoliin. Jos pyyntö epäonnistuu, tulostetaan virheilmoitus.

## Syvemmälle:

HTTP eli HyperText Transfer Protocol on protokolla, jota käytetään tietokoneiden välisessä tiedonsiirrossa. Se on ollut käytössä jo 90-luvulta lähtien ja on nykyään yleisin tapa lähettää tietoa verkon kautta. HTTP-pyynnön lisäksi on olemassa myös muita protokollia, kuten FTP ja SMTP, mutta HTTP on yleisimmin käytetty.

Muita tapoja lähettää HTTP-pyyntöjä ovat esimerkiksi JavaScriptin XMLHttpRequest ja jQueryn ajax-funktio. Näiden lisäksi on olemassa myös muita ulkopuolisia kirjastoja, jotka tarjoavat helpomman tavan lähettää pyyntöjä, kuten Axios ja Superagent.

HTTP-pyytöille on olemassa myös erilaisia metodeja, kuten GET, POST, PUT ja DELETE, jotka määrittelevät, millä tavalla pyyntöä käsitellään ja mikä on sen tarkoitus. GET-pyyntö hakee tietoa, POST-pyyntö lähettää uutta tietoa ja PUT-pyyntö päivittää olemassa olevaa tietoa. DELETE-pyyntö puolestaan poistaa tietoa.

## Katso myös:

- [MDN Web Docs - HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [w3schools - HTTP Requests](https://www.w3schools.com/js/js_ajax_http.asp)
- [Axios](https://github.com/axios/axios) ja [Superagent](https://github.com/visionmedia/superagent) - HTTP-pyyntöjen kirjastot