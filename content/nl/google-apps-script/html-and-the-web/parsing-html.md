---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:20.435928-07:00
description: "HTML parsen in Google Apps Script houdt in dat je gegevens uit HTML-content\
  \ haalt, wat vooral handig is bij interactie met webpagina's of op web\u2026"
lastmod: '2024-03-13T22:44:50.328842-06:00'
model: gpt-4-0125-preview
summary: HTML parsen in Google Apps Script houdt in dat je gegevens uit HTML-content
  haalt, wat vooral handig is bij interactie met webpagina's of op web gebaseerde
  gegevensbronnen.
title: HTML Parsen
weight: 43
---

## Wat & Waarom?
HTML parsen in Google Apps Script houdt in dat je gegevens uit HTML-content haalt, wat vooral handig is bij interactie met webpagina's of op web gebaseerde gegevensbronnen. Programmeurs doen dit om gegevensverzameling te automatiseren, webcontent te manipuleren of webfunctionaliteiten te integreren met Google Apps zoals Sheets en Docs.

## Hoe:
Google Apps Script heeft geen ingebouwde methode voor het parsen van HTML. Je kunt echter de `UrlFetchApp`-service gebruiken om HTML-content op te halen en vervolgens JavaScript-methoden of regex (reguliere expressies) gebruiken voor parsing. Hieronder staat een basisvoorbeeld van hoe je de titeltag van een webpagina kunt ophalen en parsen.

```javascript
function parseHTMLTitle(url) {
  // Haal de HTML-content van de webpagina op
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // Gebruik een simpele regex om de inhoud van de <title>-tag te vinden
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // Controleer of er een titel is gevonden en retourneer deze
  if (match && match.length > 1) {
    return match[1];
  }

  return 'Geen titel gevonden';
}

// Voorbeeldgebruik
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // Geeft de titel van de webpagina weer
```

Voor een meer geavanceerde HTML-parsing kun je de `XmlService` gebruiken om de HTML als XML te parsen. Let echter op dat dit vereist dat de HTML goed gevormde XML is, wat niet altijd het geval is:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // Navigeer vanaf hier door de XML-boom met XmlService-methoden
    // Bijvoorbeeld om een specifiek element of attribuut te vinden
  } catch(e) {
    Logger.log('Fout bij het parsen van HTML: ' + e.toString());
  }
}
```

## De diepte in:
Historisch gezien is het parsen van HTML in omgevingen zoals Google Apps Script uitdagend geweest door het gebrek aan een Document Object Model (DOM) of toegewijde parsebibliotheken die gebruikelijk zijn in andere programmeercontexten. JavaScript in een browser heeft bijvoorbeeld de DOM direct beschikbaar, en Node.js-omgevingen hebben toegang tot een overvloed aan NPM-pakketten zoals `cheerio` of `jsdom` voor het parsen van HTML.

De aanpak van Google Apps Script leunt sterk op het gebruik van `UrlFetchApp` voor webverzoeken en vervolgens het manipuleren van de responsgegevens met behulp van regex of XML-parsemethoden. Hoewel regex nuttig kan zijn voor eenvoudige parsetaken, wordt het over het algemeen niet aanbevolen voor complexe HTML vanwege het risico op fouten en de mogelijk broze aard van de code. XML-parsing met `XmlService` biedt een gestructureerdere aanpak, maar vereist goed gevormde HTML/XML, wat een beperking kan zijn bij het omgaan met willekeurige webpagina's.

Voor complexe parsebehoeften of bij omgang met slecht gevormde HTML kan een alternatieve strategie het gebruiken van een webservice buiten Google Apps Script omvatten. Deze dienst kan HTML-content verwerken met mogelijk een robuustere parseertechniek of -bibliotheek en vervolgens de verwerkte gegevens retourneren in een vorm die gemakkelijk wordt geconsumeerd door Google Apps Script. Deze aanpak introduceert echter netwerklatentie en de complexiteit van het beheren van een extra webservice.

Ondanks deze uitdagingen blijft het parsen van HTML binnen Google Apps Script een krachtig hulpmiddel, vooral in combinatie met andere Google-services en API's, waardoor een reeks automatiseringsmogelijkheden wordt geboden die de productiviteit en gegevensverwerkingsmogelijkheden aanzienlijk kunnen verhogen.
