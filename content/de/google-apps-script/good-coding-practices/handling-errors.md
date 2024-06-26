---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:56.139757-07:00
description: "Wie geht das: Google Apps Script, basierend auf JavaScript, erm\xF6\
  glicht uns die Verwendung der traditionellen `try-catch`-Anweisung f\xFCr die\u2026"
lastmod: '2024-03-13T22:44:53.342108-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, basierend auf JavaScript, erm\xF6glicht uns die Verwendung\
  \ der traditionellen `try-catch`-Anweisung f\xFCr die Fehlerbehandlung, zusammen\
  \ mit `finally`, falls eine Bereinigung unabh\xE4ngig von Erfolg oder Fehler erforderlich\
  \ ist."
title: Fehlerbehandlung
weight: 16
---

## Wie geht das:
Google Apps Script, basierend auf JavaScript, ermöglicht uns die Verwendung der traditionellen `try-catch`-Anweisung für die Fehlerbehandlung, zusammen mit `finally`, falls eine Bereinigung unabhängig von Erfolg oder Fehler erforderlich ist.

```javascript
function myFunction() {
  try {
    // Code, der einen Fehler verursachen könnte
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("Zelle A1 ist leer.");
    }
    Logger.log(data);
  } catch (e) {
    // Code zur Fehlerbehandlung
    Logger.log("Fehler: " + e.message);
  } finally {
    // Bereinigungscode, ausgeführt, ob ein Fehler aufgetreten ist oder nicht
    Logger.log("Funktion abgeschlossen.");
  }
}
```

Beispielausgabe ohne Fehler:
```
[Zellenwert]
Funktion abgeschlossen.
```

Beispielausgabe mit einem Fehler (angenommen A1 ist leer):
```
Fehler: Zelle A1 ist leer.
Funktion abgeschlossen.
```

Google Apps Script unterstützt auch das Auslösen von benutzerdefinierten Fehlern mittels des `Error`-Objekts und das Abfangen spezifischer Fehlertypen, wenn nötig. Allerdings macht die Abwesenheit einer fortgeschrittenen Fehlerkategorisierung es essentiell, sich auf Fehlermeldungen für die Spezifität zu verlassen.

## Tiefergehend
Historisch gesehen war die Fehlerbehandlung in Skriptsprachen wie JavaScript (und damit auch in Google Apps Script) weniger ausgefeilt als in einigen kompilierten Sprachen, die Funktionen wie detaillierte Ausnahme-Hierarchien und umfassende Debugging-Tools bieten. Das Modell von Google Apps Script ist relativ einfach und nutzt das Paradigma `try-catch-finally` von JavaScript. Diese Einfachheit stimmt mit dem Design der Sprache überein, kleine bis mittelgroße Anwendungen innerhalb des Ökosystems von Google schnell zu entwickeln und zu implementieren, kann jedoch Entwickler, die mit komplexen Fehlerszenarien zu tun haben, einschränken.

Bei komplexeren Anwendungen ergänzen Programmierer oft die native Fehlerbehandlung von Google Apps Script mit benutzerdefinierten Protokollierungen und Fehlerberichtsmechanismen. Dazu kann das Schreiben von Fehlern in ein Google Sheet für Audit-Zwecke gehören oder die Verwendung von Diensten Dritter zur Protokollierung durch die URL Fetch Services von Google Apps Script, um Fehlerdetails aus der Skriptumgebung zu senden.

Obwohl Google Apps Script möglicherweise hinter Sprachen wie Java oder C# in Bezug auf die eingebaute Fehlerbehandlungskomplexität und -fähigkeiten zurückbleibt, machen seine Integration in Google-Dienste und die Einfachheit des `try-catch-finally`-Ansatzes es zu einem mächtigen Werkzeug für Entwickler, um Aufgaben schnell zu automatisieren und Integrationen innerhalb des Google-Ökosystems zu erstellen. Entwickler aus anderen Bereichen stellen möglicherweise fest, dass die Herausforderung nicht darin liegt, komplexe Fehlerbehandlungsmuster zu meistern, sondern kreativ zu nutzen, was zur Verfügung steht, um sicherzustellen, dass ihre Skripte robust und benutzerfreundlich sind.
