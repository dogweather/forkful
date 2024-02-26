---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:16.484128-07:00
description: "Een HTTP-verzoek versturen in Google Apps Script gaat over het programmatisch\
  \ maken van een oproep naar een externe webserver of API. Programmeurs doen\u2026"
lastmod: '2024-02-25T18:49:47.724046-07:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek versturen in Google Apps Script gaat over het programmatisch\
  \ maken van een oproep naar een externe webserver of API. Programmeurs doen\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek versturen in Google Apps Script gaat over het programmatisch maken van een oproep naar een externe webserver of API. Programmeurs doen dit om gegevens op te halen of te versturen naar webservices, waarbij ze een breed scala aan webbronnen en functionaliteiten direct integreren in hun Google Apps Script-projecten.

## Hoe:

In Google Apps Script is de primaire manier om een HTTP-verzoek te versturen het gebruiken van de `UrlFetchApp`-service. Deze service biedt methoden om HTTP GET- en POST-verzoeken te maken. Hier is een eenvoudig voorbeeld van het maken van een GET-verzoek om JSON-gegevens op te halen:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Voor een POST-verzoek, dat vaak wordt gebruikt om gegevens naar een server te versturen, moet je meer details opnemen in de optiesparameter:

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
    // Converteer het JavaScript-object naar een JSON-string
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Deze snippets tonen basisimplementaties van GET- en POST-verzoeken. De uitvoer is afhankelijk van de API-respons en kan worden bekeken in Google Apps Script's Logger.

## Diepere duik

De `UrlFetchApp`-service van Google Apps Script is sinds de introductie aanzienlijk geëvolueerd, en biedt meer genuanceerde controle over HTTP-verzoeken met functies zoals het instellen van headers, payload en het afhandelen van multipart/form-data voor bestandsuploads. Hoewel het een eenvoudige manier biedt om externe webservices te integreren, vinden ontwikkelaars die afkomstig zijn van robuustere backendtalen zijn functionaliteit mogelijk enigszins beperkt in vergelijking met bibliotheken zoals Python's `requests` of JavaScript's `fetch` API in Node.js.

Een opmerkelijke beperking is de uitvoeringstijdlimiet voor Google Apps Script, die lange verzoeken beïnvloedt. Bovendien, terwijl `UrlFetchApp` een breed scala aan gebruiksscenario's dekt, kunnen complexere scenario's die OAuth-authenticatie of het afhandelen van zeer grote payloads vereisen, creatieve oplossingen of het benutten van aanvullende Google Cloud-bronnen nodig hebben.

Desalniettemin, voor de meeste integraties waarmee Google Workspace-ontwikkelaars te maken krijgen—variërend van automatisering van gegevensophaling tot het plaatsen van updates op externe diensten—biedt `UrlFetchApp` een krachtige, toegankelijke tool. De integratie ervan in Google Apps Script betekent dat er geen behoefte is aan externe bibliotheken of complexe setups, waardoor HTTP-verzoeken relatief eenvoudig uit te voeren zijn binnen de beperkingen van Google Apps Script. Naarmate het landschap van web-API's blijft uitbreiden, blijft `UrlFetchApp` een cruciale brug voor Google Apps Script-programma's om te interageren met de wereld buiten het ecosysteem van Google.
