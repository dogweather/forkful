---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:52.454894-07:00
description: "Das Lesen von Befehlszeilenargumenten in Google Apps Script ist etwas\
  \ irref\xFChrend, denn im Gegensatz zu traditionellen Befehlszeilenschnittstellen\
  \ in\u2026"
lastmod: '2024-03-11T00:14:27.311251-06:00'
model: gpt-4-0125-preview
summary: "Das Lesen von Befehlszeilenargumenten in Google Apps Script ist etwas irref\xFC\
  hrend, denn im Gegensatz zu traditionellen Befehlszeilenschnittstellen in\u2026"
title: Lesen von Befehlszeilenargumenten
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten in Google Apps Script ist etwas irreführend, denn im Gegensatz zu traditionellen Befehlszeilenschnittstellen in Programmiersprachen wie Python oder Node.js unterstützt Google Apps Script keine Befehlszeilenausführung oder Parameteranalyse von Natur aus. Stattdessen simulieren Programmierer diesen Prozess oft durch benutzerdefinierte Funktionen und URL-Parameter beim Ausführen von Web-Apps oder automatisierten Aufgaben, was eine dynamische Interaktion mit den Skriptfunktionalitäten basierend auf Benutzereingaben oder vordefinierten Parametern ermöglicht.

## Wie:

Um den Prozess des Lesens von Befehlszeilenargumenten in Google Apps Script nachzuahmen, besonders bei Web-Apps, können Sie Query-String-Parameter nutzen. Wenn ein Benutzer auf die URL der Web-App zugreift, können Sie Argumente wie `?name=John&age=30` anhängen und diese innerhalb Ihres Apps Script Codes parsen. So könnte die Einrichtung aussehen:

```javascript
function doGet(e) {
  var params = e.parameter; // Ruft die Query-String-Parameter ab
  var name = params['name']; // Holt den 'name' Parameter
  var age = params['age']; // Holt den 'age' Parameter

  // Beispielausgabe:
  var output = "Name: " + name + ", Alter: " + age;
  return HtmlService.createHtmlOutput(output);
}

// Beispiel-URL: https://script.google.com/macros/s/ihre_skript_id/exec?name=John&age=30
```

Wenn Sie auf die URL mit den angegebenen Parametern zugreifen, gibt das Skript etwas wie folgendes aus:

```
Name: John, Alter: 30
```

Dieser Ansatz ist ausschlaggebend für die Erstellung personalisierter Interaktionen in Web-Apps oder die programmgesteuerte Steuerung von Skriptausführungen.

## Vertiefung

Befehlszeilenargumente, wie sie im Kontext traditioneller Programmiersprachen verstanden werden, bringen die Fähigkeit für Skripte und Anwendungen mit sich, Laufzeitparameter zu verarbeiten. Dies ermöglicht flexible und dynamische Codeausführungen basierend auf Benutzereingaben oder automatisierten Prozessen. Google Apps Script, als eine cloud-basierte Skriptsprache für die leichte Anwendungsentwicklung im Google Workspace Ökosystem, operiert nicht nativ über eine Befehlszeilenschnittstelle. Stattdessen erfolgt seine Ausführung weitgehend ereignisgesteuert oder manuell ausgelöst durch die Apps Script und Google Workspace UI, oder über Web-Apps, die URL-Parameter als Pseudo-Befehlszeilenargumente parsen können.

Angesichts dieses architektonischen Unterschieds müssen Programmierer, die aus einem Hintergrund von CLI-schweren Sprachen kommen, ihren Ansatz möglicherweise anpassen, wenn sie Aufgaben automatisieren oder Anwendungen in Google Apps Script entwickeln. Statt der traditionellen Befehlszeilenargumentanalyse, kann die Nutzung der Web-App-Funktionalität von Google Apps Script oder sogar Google Sheets benutzerdefinierte Funktionen für die interaktive Datenverarbeitung ähnliche Ziele erreichen. Obwohl dies anfangs als Einschränkung erscheinen mag, fördert es die Entwicklung von benutzerfreundlicheren Schnittstellen und zugänglicheren Webanwendungen, was mit dem Fokus von Google Apps Script auf nahtloser Integration und Erweiterung von Google Workspace Anwendungen übereinstimmt.

Für Szenarien, in denen eine engere Emulation des CLI-Verhaltens von größter Bedeutung ist (z.B. das Automatisieren von Aufgaben mit dynamischen Parametern), könnten Entwickler das Nutzen externer Plattformen in Betracht ziehen, die Google Apps Script Web-Apps aufrufen und Parameter durch URLs als provisorische "Befehlszeilen" Methode übergeben. Für native Google Apps Script Projekte führt jedoch oft das Umarmen des ereignisgesteuerten und UI-zentrierten Modells der Plattform zu einfacheren und wartbareren Lösungen.
