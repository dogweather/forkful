---
aliases:
- /fi/google-apps-script/downloading-a-web-page/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:07.454080-07:00
description: "Web-sivun lataaminen Google Apps Scriptill\xE4 sis\xE4lt\xE4\xE4 sivun\
  \ sis\xE4ll\xF6n noutamisen HTML:n kautta eri tarkoituksiin, kuten verkon kaavintaan\
  \ (web scraping),\u2026"
lastmod: 2024-02-18 23:09:07.133636
model: gpt-4-0125-preview
summary: "Web-sivun lataaminen Google Apps Scriptill\xE4 sis\xE4lt\xE4\xE4 sivun sis\xE4\
  ll\xF6n noutamisen HTML:n kautta eri tarkoituksiin, kuten verkon kaavintaan (web\
  \ scraping),\u2026"
title: Verkkosivun lataaminen
---

{{< edit_this_page >}}

## Mikä & Miksi?

Web-sivun lataaminen Google Apps Scriptillä sisältää sivun sisällön noutamisen HTML:n kautta eri tarkoituksiin, kuten verkon kaavintaan (web scraping), tiedon poimintaan tai muutosten seurantaan. Ohjelmoijat valitsevat tämän operaation automatisoidakseen datankeruu- tai integraatiotehtäviä, vähentäen manuaalista työtä ja varmistaen reaaliaikaisen datan käsittelyn.

## Kuinka:

Google Apps Scriptissä `UrlFetchApp`-palvelu on keskeinen web-sisällön lataamisessa. Alla on vaiheittainen opas ja yksinkertainen esimerkki, joka näyttää kuinka noutaa ja logata verkkosivun HTML-sisältö:

1. **Perusnouto-operaatio:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Tämä koodi noutaa example.comin HTML-sisällön ja logaa sen. Se on suoraviivainen esitys siitä, kuinka saada verkkosivun lähde ilman ylimääräisiä parametreja.

2. **Uudelleenohjauksien ja HTTPS:n käsittely:**

HTTPS:n tai uudelleenohjausten käsittelyssä koodi pysyy pitkälti samana, mutta harkitse virheenkäsittelyn tai erityisasetusten toteuttamista uudelleenohjauksille:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Seuraa automaattisesti uudelleenohjauksia
    'muteHttpExceptions': true // Mykistä mahdolliset poikkeukset niiden armonvointiseksi käsittelemiseksi
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Rajoitukset ja kiintiöt:**

Ole tietoinen Google Apps Scriptin kiintiöistä; raskas käyttö voi vaatia virheenkäsittelyä nopeusrajoitusten yhteydessä.

## Syväsukellus

Historiallisesti web-sisällön lataaminen ja käsittely alkoi yksinkertaisilla HTTP-pyynnöillä, ja se on kehittynyt merkittävästi skriptikielien myötä. Google Apps Script mahdollistaa tällaisten tehtävien suoraviivaisen toteutuksen G Suite -ekosysteemissä, hyödyntäen Googlen vankkaa infrastruktuuria. `UrlFetchApp`-palvelu on tämän toiminnallisuuden ydin, kapseloiden monimutkaiset HTTP/S-pyynnöt yksinkertaisempaan sovellustason rajapintaan.

Sen mukavuudesta huolimatta Google Apps Script ei välttämättä aina ole paras työkalu raskaaseen verkkosivujen kaavintaan tai kun vaaditaan monimutkaista jälkikäsittelyä noudetulle datalle johtuen Googlen asettamista suoritusaikarajoituksista ja kiintiöistä. Tällaisissa tapauksissa omistautuneet verkkosivujen kaavintakehykset tai asynkroniseen I/O-toimintoihin suunnitellut kielet, kuten Node.js kirjastoineen kuten Puppeteer tai Cheerio, saattavat tarjota enemmän joustavuutta ja voimaa.

Lisäksi, vaikka Google Apps Script on erinomainen työkalu Google-palveluiden (kuten Sheets, Docs ja Drive) integroimiseen ja kevyiden tietonouto-operaatioiden suorittamiseen, on tärkeää pitää mielessä sen suoritusympäristön rajoitukset. Intensiivisiä tehtäviä varten harkitse Google Cloud Functionsin käyttämistä tai Apps Scriptin edistyneitä palveluita ulkoisilla laskentaresursseilla prosessointia varten.
