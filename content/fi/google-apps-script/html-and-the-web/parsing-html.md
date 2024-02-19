---
aliases:
- /fi/google-apps-script/parsing-html/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:35.617039-07:00
description: "HTML:n j\xE4sent\xE4minen Google Apps Scriptiss\xE4 tarkoittaa tietojen\
  \ poimimista HTML-sis\xE4ll\xF6st\xE4, mik\xE4 on erityisen hy\xF6dyllist\xE4, kun\
  \ toimitaan verkkosivujen tai\u2026"
lastmod: 2024-02-18 23:09:07.132445
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4minen Google Apps Scriptiss\xE4 tarkoittaa tietojen\
  \ poimimista HTML-sis\xE4ll\xF6st\xE4, mik\xE4 on erityisen hy\xF6dyllist\xE4, kun\
  \ toimitaan verkkosivujen tai\u2026"
title: "HTML:n j\xE4sent\xE4minen"
---

{{< edit_this_page >}}

## Mikä & Miksi?
HTML:n jäsentäminen Google Apps Scriptissä tarkoittaa tietojen poimimista HTML-sisällöstä, mikä on erityisen hyödyllistä, kun toimitaan verkkosivujen tai web-pohjaisten tietolähteiden kanssa. Ohjelmoijat tekevät tätä automatisoidakseen datan keräämistä, manipuloidakseen web-sisältöä tai integroidakseen web-toiminnallisuuksia Googlen sovellusten, kuten Sheets ja Docs, kanssa.

## Miten:
Google Apps Scriptillä ei ole sisäänrakennettua metodia HTML:n jäsentämiseen. Voit kuitenkin hyödyntää `UrlFetchApp`-palvelua HTML-sisällön noutamiseen ja sen jälkeen käyttää JavaScript-metodeita tai regexiä (säännöllisiä lausekkeita) jäsentämiseen. Alla on yksinkertainen esimerkki siitä, miten noutaa ja jäsentää verkkosivun title-tagi.

```javascript
function parseHTMLTitle(url) {
  // Nouda verkkosivun HTML-sisältö
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // Käytä yksinkertaista regexiä <title>-tagin sisällön löytämiseksi
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // Tarkista, löytyikö otsikko ja palauta se
  if (match && match.length > 1) {
    return match[1];
  }

  return 'Ei otsikkoa löydetty';
}

// Esimerkin käyttö
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // Tulostaa verkkosivun otsikon
```

Monimutkaisempaan HTML:n jäsentämiseen voit käyttää `XmlService`-palvelua HTML:n jäsentämiseen XML:nä. Huomaa kuitenkin, että tämä edellyttää, että HTML on hyvin muodostettua XML:ää, mikä ei aina pidä paikkaansa:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // Tästä eteenpäin navigoi XML-puuta XmlService-metodien avulla
    // Esimerkiksi löytääksesi tietyn elementin tai attribuutin
  } catch(e) {
    Logger.log('Virhe HTML:n jäsentämisessä: ' + e.toString());
  }
}
```

## Syväsukellus:
Historiallisesti HTML:n jäsentäminen ympäristöissä kuten Google Apps Script on ollut haastavaa Document Object Modelin (DOM) tai omistautuneiden jäsentämiskirjastojen puutteen vuoksi, jotka ovat yleisiä muissa ohjelmointiyhteyksissä. JavaScript selaimessa esimerkiksi tarjoaa DOM:n suoraan käytettäväksi, ja Node.js-ympäristöillä on pääsy lukuisiin NPM-paketteihin kuten `cheerio` tai `jsdom` HTML:n jäsentämiseen.

Google Apps Scriptin lähestymistapa nojaa vahvasti `UrlFetchApp`-palvelun käyttöön web-pyyntöihin ja sitten vastausdatan manipulointiin joko regexin tai XML-jäsennysmenetelmien avulla. Vaikka regex voi olla hyödyllinen yksinkertaisiin jäsentämistehtäviin, sitä ei yleensä suositella monimutkaiselle HTML:lle virheiden riskin ja koodin mahdollisesti hauraan luonteen vuoksi. XML-jäsennys `XmlService`-palvelun avulla tarjoaa rakenteellisemman lähestymistavan, mutta vaatii hyvin muodostettua HTML/XML:ää, mikä voi olla rajoitus käsiteltäessä mielivaltaisia web-sivuja.

Monimutkaisten jäsentämistarpeiden tai huonosti muodostetun HTML:n käsittelyssä yksi vaihtoehtoinen strategia voisi sisältää ulkoisen web-palvelun käyttämisen Google Apps Scriptin ulkopuolella. Tämä palvelu voisi käsitellä HTML-sisältöä mahdollisesti käyttäen vankempaa jäsentämistekniikkaa tai kirjastoa ja sitten palauttaa käsitellyn datan muodossa, joka on helposti kulutettavissa Google Apps Scriptin kautta. Tämä lähestymistapa kuitenkin tuo mukanaan verkkojen viiveitä ja lisäweb-palvelun hallinnan monimutkaisuutta.

Huolimatta näistä haasteista, HTML:n jäsentäminen Google Apps Scriptin sisällä pysyy voimakkaana työkaluna, erityisesti yhdistettynä muihin Googlen palveluihin ja API:ihin, tarjoten automaation mahdollisuuksia, jotka voivat merkittävästi parantaa tuottavuutta ja datan käsittelykykyjä.
