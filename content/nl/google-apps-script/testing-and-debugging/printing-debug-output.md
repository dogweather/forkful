---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:05.628152-07:00
description: "Hoe: Google Apps Script biedt de `Logger` klasse voor basisdebugging,\
  \ en voor meer geavanceerde behoeften, de `console` klasse ge\xEFntroduceerd in\
  \ de\u2026"
lastmod: '2024-03-13T22:44:50.334368-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script biedt de `Logger` klasse voor basisdebugging, en voor\
  \ meer geavanceerde behoeften, de `console` klasse ge\xEFntroduceerd in de V8-runtime."
title: Afdrukken van debug output
weight: 33
---

## Hoe:
Google Apps Script biedt de `Logger` klasse voor basisdebugging, en voor meer geavanceerde behoeften, de `console` klasse geïntroduceerd in de V8-runtime.

**Gebruikmakend van Logger:**

De Logger klasse stelt je in staat om debugberichten te loggen, die je na uitvoering in de Apps Script Editor kunt bekijken onder `Weergave > Logs`. Hier is een eenvoudig voorbeeld:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hallo, %s!", name);
}
```

Na het uitvoeren van `logSample()`, kun je de log bekijken met "Hallo, Wired Reader!" in de Logviewer.

**Gebruikmakend van console.log met de V8-runtime:**

Met de V8-runtime biedt `console.log` een meer vertrouwde syntax voor ontwikkelaars die uit andere talen komen:

```javascript
function consoleSample() {
  var status = 'actief';
  var count = 150;
  console.log(`Huidige status: ${status}, Aantal: ${count}`);
}
```

Na uitvoering, toegang tot de Stackdriver Logging in `Weergave > Stackdriver Logging` om de uitvoer te bekijken. Het is krachtiger, ondersteunt string interpolatie en object inspectie, en integreert met Google Cloud's logging, en biedt persistente logs en geavanceerde filtermogelijkheden.

**Voorbeelduitvoer van console.log:**

```
Huidige status: actief, Aantal: 150
```

## Diepgaande duik
Aanvankelijk was `Logger.log` het primaire hulpmiddel voor debugging in Google Apps Script, biedt een eenvoudige, rechtlijnige manier om uitvoer voor inspectie te printen. Echter, naarmate scripts complexer worden en meer geïntegreerd met Google Cloud Platform-diensten, werd de behoefte aan een robuustere logoplossing evident.

Enter de V8-runtime, die `console.log` in de vouw brengt. Dit lijnt Google Apps Script niet alleen uit met de standaard JavaScript-syntax, waardoor de taal toegankelijker wordt voor ontwikkelaars die bekend zijn met JavaScript, maar maakt ook gebruik van de krachtige infrastructuur van Google Cloud's logmogelijkheden. De introductie van `console.log` en de integratie ervan met het Google Cloud Platform markeert een significante evolutie in de debuggingmogelijkheden binnen Google Apps Script, en biedt ontwikkelaars een dynamischere en schaalbaardere benadering voor het monitoren en troubleshooten van hun scripts.

Hoewel `Logger.log` voldoende is voor basisdebuggingbehoeften en kleine projecten, biedt `console.log` met de V8-runtime een meer omvattende en toekomstbestendige oplossing. Dit omvat het vermogen om logs te behouden voorbij de uitvoeringssessie, logs te zoeken en te filteren binnen de Google Cloud-console, en de algehele uitlijning met moderne JavaScript-ontwikkelingspraktijken. Echter, ontwikkelaars moeten hun behoeften afwegen tegen de complexiteit en schaal van hun projecten wanneer ze kiezen tussen deze opties.
