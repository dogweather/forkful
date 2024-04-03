---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:04.285810-07:00
description: "Hoe te: Om een nieuw project in Google Apps Script te starten, heb je\
  \ een paar instappunten, maar laten we ons richten op de meest directe methode:\
  \ het\u2026"
lastmod: '2024-03-13T22:44:50.332205-06:00'
model: gpt-4-0125-preview
summary: Om een nieuw project in Google Apps Script te starten, heb je een paar instappunten,
  maar laten we ons richten op de meest directe methode.
title: Een nieuw project starten
weight: 1
---

## Hoe te:
Om een nieuw project in Google Apps Script te starten, heb je een paar instappunten, maar laten we ons richten op de meest directe methode: het maken van een script vanuit Google Drive.

1. **Een Project Creëren in Google Drive**
   - Navigeer naar Google Drive (drive.google.com).
   - Klik op "+ Nieuw" > "Meer" > "Google Apps Script".
   - Een nieuw scriptproject wordt geopend in de editor. Standaard bevat dit een `Code.gs`-bestand met een voorbeeldfunctie `myFunction`.

2. **Je Project Instellen**
   - Hernoem je project voor duidelijkheid. Klik linksboven op "Naamloos project" en geef het een betekenisvolle naam.
   - Schrijf een eenvoudige functie in het `Code.gs`-bestand om er gevoel bij te krijgen:

```javascript
function helloWorld() {
  Logger.log('Hallo, wereld!');
}
```

   - Voer `helloWorld` uit door de functie te selecteren in het dropdown-menu naast de afspeelknop (▶) en erop te klikken. Hierdoor wordt de functie uitgevoerd.

3. **Logs Bekijken**
   - Om de uitvoer van `Logger.log` te bekijken, ga je naar "Beeld" > "Logs", of druk je op `Ctrl + Enter`. Je zou "Hallo, wereld!" in de logs moeten zien.

Gefeliciteerd, je hebt zojuist succesvol een nieuw project in Google Apps Script gestart en een eenvoudige functie uitgevoerd!

## Diepgaand
De introductie van Google Apps Script rond 2009 bood een krachtig maar toegankelijk platform voor zowel ontwikkelaars als niet-ontwikkelaars om te automatiseren, uit te breiden en voort te bouwen op de uitgebreide reeks van Google-services. In tegenstelling tot traditionele programmeeromgevingen, biedt GAS een unieke combinatie van eenvoud en integratie, direct binnen het Google-ecosysteem, zonder de noodzaak voor externe servers of configuratie. Dit serverloze uitvoeringsmodel vereenvoudigt de projectimplementatie en -beheer aanzienlijk.

Historisch gezien was GAS enigszins beperkt door zijn uitvoeringsomgeving en taalversie, vaak achterlopend op de huidige JavaScript-standaarden. Echter, recente updates hebben moderne JavaScript-syntaxis (ECMAScript 2015+) naar GAS gebracht, waardoor het aantrekkelijker is voor ontwikkelaars die gewend zijn aan hedendaagse ontwikkelpraktijken.

Hoewel GAS uniek gepositioneerd is om te interageren met Google Services, zijn er alternatieve benaderingen voor meer intensieve of specifieke behoeften. Bijvoorbeeld, Google Cloud Functions en Google Cloud Platform (GCP) bieden robuustere en schaalbaardere oplossingen voor het afhandelen van complexe workflows, het verwerken van grote datasets en het integreren met externe API's. Deze platforms staan programmeren in verschillende talen toe (bijv. Python, Go, Node.js) en bieden grotere computationele bronnen.

Niettemin, voor taken die nauw verbonden zijn met Google Apps, automatisering en snelle ontwikkeling binnen dit ecosysteem, blijft Google Apps Script een ongeëvenaard hulpmiddel in termen van gebruiksgemak en integratiediepte. De toegankelijkheid ervan direct vanuit Google Drive en de naadloze verbinding met Google-services maken het een praktische keuze voor een breed scala aan projecten, met name voor diegenen die de functionaliteit van Spreadsheets, Documenten, Formulieren en andere Google-applicaties willen uitbreiden.
