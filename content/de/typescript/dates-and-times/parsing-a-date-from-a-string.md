---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:35.761771-07:00
description: "Das Parsen eines Datums aus einem String beinhaltet das Umwandeln von\
  \ textuellen Darstellungen von Daten und Zeiten in ein Format, das vom Programm\u2026"
lastmod: '2024-03-13T22:44:53.640223-06:00'
model: gpt-4-0125-preview
summary: Das Parsen eines Datums aus einem String beinhaltet das Umwandeln von textuellen
  Darstellungen von Daten und Zeiten in ein Format, das vom Programm manipuliert und
  analysiert werden kann.
title: Einen Datum aus einem String analysieren
weight: 30
---

## Was & Warum?
Das Parsen eines Datums aus einem String beinhaltet das Umwandeln von textuellen Darstellungen von Daten und Zeiten in ein Format, das vom Programm manipuliert und analysiert werden kann. Dies ist eine gängige Aufgabe in der Programmierung, da sie die Bearbeitung von Benutzereingaben, die Speicherung von zeitgestempelten Daten und Interaktionen mit APIs ermöglicht, was zu funktionaleren und benutzerfreundlicheren Anwendungen führt.

## Wie:
TypeScript, als Obermenge von JavaScript, stützt sich auf das Date-Objekt zum Parsen von Daten aus Strings. Allerdings kann das Arbeiten mit Daten in JS/TS aufgrund der Eigenarten des Date-Objekts umständlich oder ungenau werden. Hier ist ein einfaches Beispiel gefolgt von einem Ansatz unter Verwendung der beliebten Bibliothek `date-fns`, für robustere Lösungen.

### Verwendung des Date-Objekts von JavaScript
```typescript
// Einfaches Parsen mit dem Date-Konstruktor
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// Ausgabe für GMT: "Fri Apr 21 2023 15:00:00 GMT+0000 (Koordinierte Weltzeit)"
```

Diese Methode funktioniert für ISO-Format-Strings und einige andere Datenformate, kann jedoch bei mehrdeutigen Formaten über Browser und Lokalitäten hinweg inkonsistente Ergebnisse liefern.

### Verwendung von date-fns
Die Bibliothek `date-fns` bietet eine unkomplizierte und konsistente Bearbeitung von Daten. Sie ist eine modulare Bibliothek, was bedeutet, dass Sie nur die Teile einbeziehen können, die Sie benötigen, und so die Bundle-Größe reduzieren.

Zuerst `date-fns` installieren:

```sh
npm install date-fns
```

Danach verwenden Sie es, um einen Datum-String zu parsen:

```typescript
import { parseISO, format } from 'date-fns';

// Parsen eines ISO-Strings
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// Formatieren des Datums (z.B. in eine menschenlesbare Form)
console.log(format(parsedDate, "PPPpp")); 
// Ausgabe: "Apr 21st, 2023 at 3:00 PM" (Die Ausgabe kann je nach Lokalität variieren)
```

`date-fns` unterstützt eine breite Vielfalt an Formaten und Lokalitäten, was es zu einer robusten Wahl für Anwendungen macht, die präzises Parsen und Formatieren von Daten über verschiedene Benutzerregionen hinweg erfordern.
