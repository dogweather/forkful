---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:33.436627-07:00
description: "De huidige datum krijgen in Google Apps Script gaat over het ophalen\
  \ van de live datum en tijd, een gebruikelijke taak voor het automatiseren van taken,\u2026"
lastmod: '2024-03-11T00:14:24.152359-06:00'
model: gpt-4-0125-preview
summary: "De huidige datum krijgen in Google Apps Script gaat over het ophalen van\
  \ de live datum en tijd, een gebruikelijke taak voor het automatiseren van taken,\u2026"
title: Het verkrijgen van de huidige datum
---

{{< edit_this_page >}}

## Wat & Waarom?

De huidige datum krijgen in Google Apps Script gaat over het ophalen van de live datum en tijd, een gebruikelijke taak voor het automatiseren van taken, loggen, en timestamping in apps die verbonden zijn met Google's ecosysteem. Programmeurs gebruiken dit voor het genereren van dynamische inhoud, deadline tracking, en planning binnen Google Docs, Sheets en andere Google-diensten.

## Hoe:

Google Apps Script, dat is gebaseerd op JavaScript, biedt eenvoudige methoden om de huidige datum te krijgen. Je kunt de `new Date()` constructor gebruiken om een nieuw datumobject te creëren dat de huidige datum en tijd vertegenwoordigt. Hier is hoe je dit kunt manipuleren en weergeven in verschillende formaten.

```javascript
function toonHuidigeDatum() {
  var huidigeDatum = new Date();
  
  Logger.log(huidigeDatum); // Logt de huidige datum en tijd in de tijdzone van het script
  
  // Om alleen de datum weer te geven in het formaat JJJJ-MM-DD
  var datumString = huidigeDatum.getFullYear() + '-' + 
                   (huidigeDatum.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   huidigeDatum.getDate().toString().padStart(2, '0');
  Logger.log(datumString); // Voorbeelduitvoer: "2023-04-01"
  
  // Weergeven in een meer leesbaar formaat
  var opties = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var leesbareDatum = huidigeDatum.toLocaleDateString('en-US', opties) + ' ' + 
                     huidigeDatum.toLocaleTimeString('en-US', opties);
                     
  Logger.log(leesbareDatum); // Voorbeelduitvoer: "1 april 2023, 12:00:00 PM GMT+1"
}
```

Deze codevoorbeelden laten zien hoe je de huidige datum en tijd kunt vastleggen en vormgeven, waarmee de veelzijdigheid voor verschillende programmeerbehoeften binnen Google Apps Script wordt getoond.

## Diepere Duik

Voordat JavaScript zich vestigde op het `Date` object, moesten programmeurs handmatig de tijd en datum bijhouden door middel van minder standaard en omslachtigere middelen. Dit omvatte het gebruik van timestamp integers en zelfgemaakte datumfuncties, die varieerden van de ene programmeeromgeving naar de andere, wat leidde tot inconsistentie en compatibiliteitsproblemen.

De introductie van het `new Date()` object in JavaScript, en bij uitbreiding Google Apps Script, standaardiseerde datum- en tijdoperaties, maakte ze intuïtiever en verminderde de hoeveelheid code die nodig is voor datumgerelateerde bewerkingen. Het is belangrijk om op te merken dat hoewel de implementatie in Google Apps Script handig en voldoende is voor veel toepassingen binnen Google's suite van producten, het misschien niet voldoet aan alle scenario's, vooral die welke complexe tijdzonebehandeling of precieze tijd-stempelregistratie vereisen in snel veranderende omgevingen.

Voor dergelijke geavanceerde gebruiksscenario's wenden programmeurs zich vaak tot bibliotheken zoals Moment.js of date-fns in JavaScript. Hoewel Google Apps Script deze bibliotheken niet native ondersteunt, kunnen ontwikkelaars enkele van hun functionaliteiten nabootsen met behulp van beschikbare JavaScript-datummethode of door toegang te krijgen tot externe bibliotheken via HTML Service of Apps Script's URL Fetch-service. Ondanks deze alternatieven blijft de eenvoud en integratie van de native datum- en tijdfuncties van Google Apps Script de eerste keuze voor de meeste taken binnen het Google-ecosysteem.
