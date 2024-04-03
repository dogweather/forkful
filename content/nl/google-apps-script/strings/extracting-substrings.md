---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:55.343624-07:00
description: "Het extraheren van substrings houdt in dat een deel van een string wordt\
  \ genomen - in wezen het cre\xEBren van een nieuwe string uit een gedeelte van een\u2026"
lastmod: '2024-03-13T22:44:50.319489-06:00'
model: gpt-4-0125-preview
summary: "Het extraheren van substrings houdt in dat een deel van een string wordt\
  \ genomen - in wezen het cre\xEBren van een nieuwe string uit een gedeelte van een\
  \ bestaande."
title: Substrings extraheren
weight: 6
---

## Wat & Waarom?

Het extraheren van substrings houdt in dat een deel van een string wordt genomen - in wezen het creëren van een nieuwe string uit een gedeelte van een bestaande. Programmeurs doen dit om tal van redenen, waaronder gegevensanalyse, tekstmanipulatie voor gebruikersinterfaces of het verwerken van invoer voor verschillende toepassingen, waardoor substringextractie een veelzijdig instrument is in elk scriptarsenaal.

## Hoe:

In Google Apps Script, dat is gebaseerd op modern JavaScript, kan substringextractie worden bereikt met verschillende methoden, waaronder `substring()`, `substr()`, en `slice()`. Elk heeft zijn nuances, maar ze dienen allemaal het doel om gespecificeerde karakters uit een string te halen.

```javascript
// Voorbeeld met substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Uitvoer: Hello

// Voorbeeld met substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Uitvoer: world

// Voorbeeld met slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Uitvoer: world!
```

Elke methode neemt twee argumenten: de startpositie en, behalve `slice()` die negatieve indices kan accepteren om vanaf het einde te beginnen, de eindpositie of het aantal te extraheren karakters. Het is de moeite waard om te benadrukken dat de originele string ongewijzigd blijft na deze operaties, aangezien ze nieuwe stringwaarden teruggeven.

## Diepgaande Duik

Historisch gezien zijn de JavaScript-methoden voor het extraheren van substrings een bron van verwarring geweest door hun vergelijkbare namen en functionaliteiten. Echter, in Google Apps Script en modern JavaScript worden `substring()` en `slice()` het meest gebruikt, met `substr()` dat als verouderd beschouwd wordt. Dit is belangrijk om te onthouden voor wie toekomstbestendige code schrijft.

Het belangrijkste verschil tussen `substring()` en `slice()` is hoe ze omgaan met negatieve indices; `substring()` behandelt negatieve indices als 0, terwijl `slice()` een negatieve index kan accepteren om de extractie vanaf het einde van de string te starten. Dit maakt `slice()` bijzonder handig voor gevallen waar de exacte lengte van de string misschien niet bekend is of wanneer er vanaf het einde moet worden geëxtraheerd.

Bij het beslissen welke methode te gebruiken voor substringextractie, komt de keuze vaak neer op de specifieke vereisten van de operatie (bijv. of het omgaan met negatieve indices voordelig is) en persoonlijke of teamcoderingsstandaarden. Hoewel er geen one-size-fits-all beste praktijk is, kan het begrijpen van de subtiele verschillen en prestatie-implicaties helpen om een geïnformeerde beslissing te maken.
