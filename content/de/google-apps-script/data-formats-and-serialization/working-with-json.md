---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:20.480417-07:00
description: "JSON oder JavaScript Object Notation ist ein leichtgewichtiges Format\
  \ f\xFCr die Speicherung und den Transport von Daten, ideal f\xFCr die Kommunikation\
  \ von\u2026"
lastmod: 2024-02-19 22:05:12.404677
model: gpt-4-0125-preview
summary: "JSON oder JavaScript Object Notation ist ein leichtgewichtiges Format f\xFC\
  r die Speicherung und den Transport von Daten, ideal f\xFCr die Kommunikation von\u2026"
title: Arbeiten mit JSON
---

{{< edit_this_page >}}

## Was & Warum?

JSON oder JavaScript Object Notation ist ein leichtgewichtiges Format für die Speicherung und den Transport von Daten, ideal für die Kommunikation von Server zu Client und für Konfigurationsdateien. Programmierer nutzen es in Google Apps Script für den nahtlosen Datenaustausch zwischen Google-Diensten (wie Sheets, Docs, Drive) und externen Quellen aufgrund seiner menschenlesbaren Struktur und einfachen Integration in JavaScript-basierte Umgebungen.

## Wie:

In Google Apps Script ist die Manipulation von JSON ein unkomplizierter Prozess, größtenteils dank der nativen Unterstützung, die JavaScript für das Parsen und Stringifizieren von JSON bietet. Hier sind einige gängige Operationen:

**1. Parsen von JSON**: Nehmen wir an, wir erhalten eine JSON-Zeichenkette von einem Webdienst; es ist wesentlich, dass wir sie in ein JavaScript-Objekt parsen, um Daten manipulieren zu können.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Ausgabe: Sample Project
```

**2. Stringifizieren von JavaScript-Objekten**: Umgekehrt ist die Konvertierung eines JavaScript-Objekts in eine JSON-Zeichenkette nützlich, wenn wir Daten von Apps Script an einen externen Dienst senden müssen.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Ausgabe: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Arbeiten mit komplexen Daten**:
Für komplexere Datenstrukturen, wie Arrays von Objekten, bleibt der Prozess der gleiche, was die Flexibilität von JSON für die Datenrepräsentation unterstreicht.

```javascript
var projects = [
  {name: "Projekt 1", version: "1.0"},
  {name: "Projekt 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Ausgabe: '[{"name":"Projekt 1","version":"1.0"},{"name":"Projekt 2","version":"2.0"}]'
```

## Tiefergehend

Die Allgegenwart von JSON in modernen Webanwendungen kann nicht genug betont werden, verwurzelt in seiner Einfachheit und wie nahtlos es sich in JavaScript, der Sprache des Webs, integriert. Sein Design, inspiriert von JavaScript-Objektliteralen, allerdings strenger, erleichtert seine rasche Annahme. Im frühen 21. Jahrhundert gewann JSON als Alternative zu XML für AJAX-getriebene Webanwendungen an Popularität, da es ein leichtgewichtigeres und weniger umständliches Dateninterchange-Format bot. Angesichts der tiefen Integration von Google Apps Script mit verschiedenen Google-APIs und externen Diensten dient JSON als zentrales Format für die Strukturierung, den Transport und die Manipulation von Daten über diese Plattformen hinweg.

Während JSON für Webanwendungen die Oberhand gewinnt, existieren alternative Datenformate wie YAML für Konfigurationsdateien oder Protobuf für effizientere binäre Serialisierung in Hochleistungsumgebungen. Jedoch festigt die Balance aus Lesbarkeit, Benutzerfreundlichkeit und breiter Unterstützung über Programmiersprachen und Werkzeuge hinweg JSONs Position als die Standardwahl für viele Entwickler, die sich in Google Apps Script und darüber hinaus wagen.
