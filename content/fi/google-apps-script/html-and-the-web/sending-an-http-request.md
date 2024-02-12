---
title:                "HTTP-pyynnön lähettäminen"
date:                  2024-02-01T22:01:19.033902-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTTP-pyynnön lähettäminen"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/google-apps-script/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

HTTP-pyynnön lähettäminen Google Apps Scriptillä tarkoittaa ohjelmallisesti kutsun tekemistä ulkoiselle verkkopalvelimelle tai API:lle. Ohjelmoijat tekevät tämän hakeakseen tai lähettääkseen tietoja verkkopalveluihin, yhdistäen valtavan määrän verkkoresursseja ja toiminnallisuuksia suoraan heidän Google Apps Script -projekteihinsa.

## Kuinka:

Google Apps Scriptissä HTTP-pyynnön lähettämisen ensisijainen keino on käyttämällä `UrlFetchApp`-palvelua. Tämä palvelu tarjoaa metodeja HTTP GET- ja POST-pyyntöjen tekemiseen. Tässä on yksinkertainen esimerkki GET-pyynnön tekemisestä JSON-tiedon noutamiseen:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

POST-pyynnölle, jota käytetään yleisesti tietojen lähettämiseen palvelimelle, sinun on sisällytettävä lisätietoja options-parametriin:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Muunna JavaScript-objekti JSON-merkkijonoksi
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Nämä katkelmat näyttävät perus GET- ja POST-pyynnön toteutukset. Tuloste riippuu API:n vastauksesta ja sitä voidaan tarkastella Google Apps Scriptin Logissa.

## Syväsukellus

Google Apps Scriptin `UrlFetchApp`-palvelu on kehittynyt merkittävästi sen alkuvaiheista, tarjoten tarkempaa hallintaa HTTP-pyyntöihin ominaisuuksilla, kuten otsikoiden asettaminen, payload ja multipart/form-data-käsittely tiedostojen latauksille. Vaikka se tarjoaa suoraviivaisen keinon integroida ulkoiset verkkopalvelut, kehittäjät, jotka tulevat robottimaisemmista taustajärjestelmäkielistä, saattavat pitää sen toiminnallisuutta jossain määrin rajoittuneena verrattuna kirjastoihin, kuten Pythonin `requests` tai JavaScriptin `fetch` API Node.js:ssä.

Yksi merkittävä rajoitus on Google Apps Scriptin suoritusaikaraja, joka vaikuttaa pitkäkestoisia pyyntöjä. Lisäksi, vaikka `UrlFetchApp` kattaa laajan valikoiman käyttötapauksia, monimutkaisemmat skenaariot, jotka liittyvät OAuth-autentikointiin tai erittäin suurten kuormien käsittelyyn, saattavat vaatia luovia ratkaisuja tai lisäyksiä Google Cloud -resursseista.

Siitä huolimatta, useimpien integraatioiden osalta, joita Google Workspace -kehittäjät kohtaavat – muun muassa datan automaattinen hankinta ja päivitysten lähettäminen ulkoisiin palveluihin – `UrlFetchApp` tarjoaa tehokkaan, saavutettavan työkalun. Sen integrointi Google Apps Scriptiin tarkoittaa, ettei ulkoisia kirjastoja tai monimutkaista asetusta tarvita, mikä tekee HTTP-pyyntöjen suorittamisesta suhteellisen suoraviivaista Google Apps Scriptin rajoitusten puitteissa. Kun verkkoliittymien maisema jatkaa laajentumistaan, `UrlFetchApp` pysyy keskeisenä silta Google Apps Script -ohjelmille toimia vuorovaikutuksessa Googlen ekosysteemin ulkopuolisen maailman kanssa.
