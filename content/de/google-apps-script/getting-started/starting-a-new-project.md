---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:27.507025-07:00
description: "Ein neues Projekt in Google Apps Script (GAS) zu starten, bedeutet,\
  \ eine Skriptdatei innerhalb des Google-\xD6kosystems (Google Drive, Docs, Sheets\
  \ usw.) zu\u2026"
lastmod: '2024-03-13T22:44:53.334668-06:00'
model: gpt-4-0125-preview
summary: "Ein neues Projekt in Google Apps Script (GAS) zu starten, bedeutet, eine\
  \ Skriptdatei innerhalb des Google-\xD6kosystems (Google Drive, Docs, Sheets usw."
title: Einen neuen Projektstart
weight: 1
---

## Was & Warum?

Ein neues Projekt in Google Apps Script (GAS) zu starten, bedeutet, eine Skriptdatei innerhalb des Google-Ökosystems (Google Drive, Docs, Sheets usw.) zu initialisieren, um Aufgaben zu automatisieren oder die Funktionalitäten von Google Apps zu erweitern. Programmierer begeben sich oft auf diese Reise, um Arbeitsabläufe zu optimieren, Google-Dienste programmatisch zu manipulieren oder benutzerdefinierte Add-ons zu erstellen, wodurch Zeit gespart und die Leistungsfähigkeit von Googles Infrastruktur genutzt wird.

## Wie:

Um ein neues Projekt in Google Apps Script zu starten, haben Sie einige Einstiegspunkte, aber konzentrieren wir uns auf die direkteste Methode: das Erstellen eines Skripts aus Google Drive.

1. **Ein Projekt in Google Drive erstellen**
   - Navigieren Sie zu Google Drive (drive.google.com).
   - Klicken Sie auf "+ Neu" > "Mehr" > "Google Apps Script".
   - Ein neues Skriptprojekt öffnet sich im Editor. Standardmäßig enthält es eine `Code.gs`-Datei mit einer Beispiel `myFunction`.

2. **Ihr Projekt einrichten**
   - Benennen Sie Ihr Projekt zur besseren Übersicht um. Klicken Sie oben links auf "Unbenanntes Projekt" und geben Sie ihm einen aussagekräftigen Namen.
   - Schreiben Sie eine einfache Funktion in der `Code.gs`-Datei, um ein Gefühl dafür zu bekommen:

```javascript
function helloWorld() {
  Logger.log('Hallo, Welt!');
}
```

   - Führen Sie `helloWorld` aus, indem Sie die Funktion im Dropdown-Menü neben dem Abspielknopf (▶) auswählen und darauf klicken. Dies führt die Funktion aus.

3. **Logs ansehen**
   - Um die Ausgabe von `Logger.log` zu sehen, gehen Sie zu "Ansicht" > "Protokolle", oder drücken Sie `Strg + Eingabe`. Sie sollten "Hallo, Welt!" in den Protokollen sehen.

Herzlichen Glückwunsch, Sie haben gerade erfolgreich ein neues Projekt in Google Apps Script gestartet und eine einfache Funktion ausgeführt!

## Vertiefung

Die Einführung von Google Apps Script um das Jahr 2009 herum bot eine leistungsstarke und dennoch zugängliche Plattform sowohl für Entwickler als auch für Nicht-Entwickler, um das umfangreiche Angebot an Google-Diensten zu automatisieren, zu erweitern und darauf aufzubauen. Im Gegensatz zu traditionellen Programmierumgebungen bietet GAS eine einzigartige Kombination aus Einfachheit und Integration direkt im Google-Ökosystem, ohne die Notwendigkeit externer Server oder Einrichtungen. Dieses serverlose Ausführungsmodell vereinfacht die Projektabwicklung und -verwaltung erheblich.

Historisch gesehen war GAS durch sein Ausführungsumfeld und die Version der Sprache etwas eingeschränkt und hinkte oft hinter den aktuellen JavaScript-Standards hinterher. Jedoch haben jüngste Updates modernen JavaScript-Syntax (ECMAScript 2015+) zu GAS gebracht, was es für Entwickler, die an zeitgenössische Entwicklungspraktiken gewöhnt sind, attraktiver macht.

Obwohl GAS einzigartig positioniert ist, um mit Google Services zu interagieren, gibt es alternative Ansätze für intensivere oder spezifische Bedürfnisse. Beispielsweise bieten Google Cloud Functions und die Google Cloud Platform (GCP) robustere und skalierbarere Lösungen für die Handhabung komplexer Arbeitsabläufe, die Verarbeitung großer Datensätze und die Integration mit externen APIs. Diese Plattformen ermöglichen die Programmierung in verschiedenen Sprachen (z.B. Python, Go, Node.js) und bieten größere Rechenressourcen.

Dennoch bleibt Google Apps Script für Aufgaben, die eng mit Google Apps, Automatisierung und schneller Entwicklung innerhalb dieses Ökosystems verbunden sind, ein unübertroffenes Werkzeug hinsichtlich Benutzerfreundlichkeit und Integrationstiefe. Seine Zugänglichkeit direkt aus Google Drive und die nahtlose Verbindung zu Google-Diensten machen es zu einer praktischen Wahl für eine breite Palette von Projekten, insbesondere für diejenigen, die die Funktionalität von Sheets, Docs, Formularen und anderen Google-Anwendungen erweitern möchten.
