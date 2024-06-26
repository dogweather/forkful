---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:17.982887-07:00
description: "Hur man g\xF6r: I Google Apps Script kan loggning utf\xF6ras med olika\
  \ metoder, s\xE5som `Logger`-klassen och `console.log()`. Logger-klassen \xE4r det\
  \ traditionella\u2026"
lastmod: '2024-03-13T22:44:37.445878-06:00'
model: gpt-4-0125-preview
summary: "I Google Apps Script kan loggning utf\xF6ras med olika metoder, s\xE5som\
  \ `Logger`-klassen och `console.log()`."
title: Loggning
weight: 17
---

## Hur man gör:
I Google Apps Script kan loggning utföras med olika metoder, såsom `Logger`-klassen och `console.log()`. Logger-klassen är det traditionella sättet, lämpligt för enkel felsökning och utvecklingsändamål. Sedan senaste uppdateringarna erbjuder `console.log()` mer flexibilitet och integration med Stackdriver Logging, vilket ger en mer robust lösning för övervakning av dina Apps-skript i Google Cloud Platform.

**Använda Logger:**

```javascript
function logSample() {
  Logger.log('Detta är ett enkelt loggmeddelande');
  
  var value = 5;
  Logger.log('Värdet är: %s', value); // Strängformatering
}

// För att visa loggen:
// 1. Kör funktionen logSample.
// 2. Visa -> Loggar
```

**Exempel på Logger-utdata:**

```
[22-04-20 10:00:00:000 PDT] Detta är ett enkelt loggmeddelande
[22-04-20 10:00:00:001 PDT] Värdet är: 5
```

**Använda console.log():**

```javascript
function consoleLogSample() {
  console.log('Detta meddelande går till Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Utvecklare'};
  console.info('Loggar ett objekt:', obj);
}

// Loggar kan visas i Google Cloud Platform (GCP) konsolen under Stackdriver Logging
```

**Exempel på console.log()-utdata:**

```
Detta meddelande går till Stackdriver Logging
Loggar ett objekt: {name: "Jane", role: "Utvecklare"}
```

Genom att övergå till `console.log()` för komplexa applikationer kan utvecklare effektivt analysera och sortera loggar med de kraftfulla filter och verktyg som tillhandahålls av GCP, vilket inte är lika okomplicerat med den traditionella Logger-klassen.

## Djupdykning:
Loggning i Google Apps Script har utvecklats avsevärt. Inledningsvis var `Logger`-klassen den primära metoden för utvecklare att felsöka sina skript. Den är enkel och tillräcklig för grundläggande skript, men saknar de funktioner som behövs för moderna molnapplikationer, såsom att söka i loggar eller analysera loggtrender över tid.

Introduktionen av `console.log()` överbryggade detta gap genom att integrera loggning i Google Apps Script med Google Clouds Stackdriver Logging (nu kallad Operations Suite), vilket tillhandahåller en centraliserad plattform för loggning, övervakning och felsökning av applikationer. Detta tillät inte bara loggning i stor skala, utan öppnade också upp avancerade funktioner för logghantering som loggbaserade metriker, analys av loggar i realtid och integration med andra tjänster i Google Cloud.

Medan `Logger` fortfarande tjänar ett syfte för snabb felsökning och loggning i mindre skript, reflekterar utvecklingen mot användning av `console.log()` en bredare trend i utvecklingen av skalbara, molnbaserade applikationer. Det understryker Googles engagemang i att tillhandahålla utvecklare med verktyg som möter komplexiteten och skalan i dagens applikationer. Trots detta bör nykomlingar vara medvetna om den något brantare inlärningskurvan och nödvändigheten av att bekanta sig med koncepten i Google Cloud Platform. Trots detta är övergången fördelaktig för utvecklare som ser att fullt ut utnyttja molnets möjligheter. Denna anpassning till molntjänster är en del av en bredare trend i programvaruutveckling, som betonar vikten av robusta, skalbara loggningsmekanismer i molnberäkningens era.
