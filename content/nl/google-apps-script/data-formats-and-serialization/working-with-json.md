---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:34.919824-07:00
description: "JSON, of JavaScript Object Notation, is een lichtgewicht formaat voor\
  \ het opslaan en transporteren van gegevens, ideaal voor communicatie van server\
  \ naar\u2026"
lastmod: '2024-03-13T22:44:50.355055-06:00'
model: gpt-4-0125-preview
summary: JSON, of JavaScript Object Notation, is een lichtgewicht formaat voor het
  opslaan en transporteren van gegevens, ideaal voor communicatie van server naar
  client en configuratiebestanden.
title: Werken met JSON
weight: 38
---

## Wat & Waarom?

JSON, of JavaScript Object Notation, is een lichtgewicht formaat voor het opslaan en transporteren van gegevens, ideaal voor communicatie van server naar client en configuratiebestanden. Programmeurs maken er gebruik van in Google Apps Script voor naadloze gegevensuitwisseling tussen Google-diensten (zoals Sheets, Docs, Drive) en externe bronnen, vanwege de leesbare structuur en eenvoudige integratie binnen op JavaScript gebaseerde omgevingen.

## Hoe te:

In Google Apps Script is het manipuleren van JSON een eenvoudig proces, grotendeels dankzij de native ondersteuning die JavaScript biedt voor het parseren en stringificeren van JSON. Hier zijn enkele veelvoorkomende bewerkingen:

**1. JSON Parseren**: Stel dat we een JSON-tekenreeks ophalen van een webservice; het parseren naar een JavaScript-object is essentieel voor gegevensmanipulatie.

```javascript
var jsonString = '{"name": "Voorbeeldproject", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Output: Voorbeeldproject
```

**2. JavaScript Objecten Stringificeren**: Omgekeerd is het converteren van een JavaScript-object naar een JSON-tekenreeks nuttig wanneer we gegevens van Apps Script naar een externe dienst moeten sturen.

```javascript
var projectData = {
  name: "Voorbeeldproject",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Output: '{"name":"Voorbeeldproject","version":"1.0.0"}'
```

**3. Werken met Complexe Gegevens**:
Voor complexere gegevensstructuren, zoals arrays van objecten, blijft het proces hetzelfde, waarmee de flexibiliteit van JSON voor gegevensrepresentatie wordt aangetoond.

```javascript
var projecten = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projecten);
Logger.log(jsonString); // Output: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Diep Duiken

De alomtegenwoordigheid van JSON in moderne webapplicaties kan niet worden onderschat, geworteld in zijn eenvoud en hoe naadloos het integreert met JavaScript, de taal van het web. Het ontwerp, geïnspireerd door JavaScript-objectliteralen, zij het strikter, vergemakkelijkt de snelle adoptie ervan. In het begin van de jaren 2000 won JSON aan populariteit als een alternatief voor XML voor op AJAX gebaseerde webapplicaties, en bood een lichter en minder omslachtig gegevensuitwisselformaat. Gezien de diepe integratie van Google Apps Script met diverse Google API's en externe diensten, fungeert JSON als een cruciaal formaat voor het structureren, transporteren en manipuleren van gegevens over deze platforms.

Terwijl JSON de boventoon voert voor webapplicaties, bestaan er alternatieve gegevensformaten zoals YAML voor configuratiebestanden of Protobuf voor efficiëntere binaire serialisatie in prestatie-intensieve omgevingen. Echter, de balans van JSON tussen leesbaarheid, gebruiksgemak en brede ondersteuning in programmeertalen en tools verstevigt zijn positie als de standaardkeuze voor veel ontwikkelaars die zich in Google Apps Script en daarbuiten wagen.
