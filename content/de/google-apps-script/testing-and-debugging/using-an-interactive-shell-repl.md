---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:47.522797-07:00
description: "Eine interaktive Shell oder Read-Eval-Print Loop (REPL) ist eine einfache,\
  \ interaktive Programmierumgebung, die einzelne Benutzereingaben (Ausdr\xFCcke)\u2026"
lastmod: '2024-03-11T00:14:27.295457-06:00'
model: gpt-4-0125-preview
summary: "Eine interaktive Shell oder Read-Eval-Print Loop (REPL) ist eine einfache,\
  \ interaktive Programmierumgebung, die einzelne Benutzereingaben (Ausdr\xFCcke)\u2026"
title: Verwendung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?

Eine interaktive Shell oder Read-Eval-Print Loop (REPL) ist eine einfache, interaktive Programmierumgebung, die einzelne Benutzereingaben (Ausdrücke) entgegennimmt, diese auswertet und das Ergebnis an den Benutzer zurückgibt. Programmierer nutzen REPLs für schnelles Prototyping, Debugging und um die Syntax und das Verhalten einer Programmiersprache interaktiv zu erlernen.

## Wie:

Google Apps Script, eine cloud-basierte Skriptsprache zum Automatisieren von Aufgaben über Google-Produkte hinweg, verfügt über kein eingebautes REPL-Tool ähnlich denen in Sprachen wie Python oder JavaScripts Node.js. Jedoch können Sie eine ähnliche Erfahrung simulieren, indem Sie die Protokollierungs- und Debugging-Funktionen des Apps Script Editors nutzen oder eine externe Umgebung einrichten. Hier konzentrieren wir uns darauf, ein provisorisches REPL innerhalb des Apps Script Editors zu erstellen.

1. **Erstellen einer provisorischen REPL-Funktion**:

```javascript
function myREPL() {
  var input = Logger.log('Geben Sie Ihren Ausdruck ein: ');
  try {
    var result = eval(input);
    Logger.log('Ergebnis: ' + result);
  } catch(e) {
    Logger.log('Fehler: ' + e.message);
  }
}
```

Da direkte Benutzereingaben im Apps Script-Umfeld nicht auf die gleiche Weise wie in einem traditionellen REPL möglich sind, können Sie die Variable `input` manuell ändern und `myREPL()` ausführen, um Ausdrücke zu testen.

2. **Beispielhafte Codeausführung**:

Nehmen wir an, Sie möchten `2+2` bewerten. Sie würden die `myREPL`-Funktion wie folgt ändern:

```javascript
function myREPL() {
  var input = '2+2'; // Geben Sie hier Ihren Ausdruck manuell ein
  // Der Rest bleibt gleich...
}
```

Nach dem Ausführen von `myREPL()`, überprüfen Sie die Protokolle (Ansicht > Protokolle) auf die Ausgabe, die etwa so lauten sollte:

```
[20-xx-xxxx xx:xx:xx:xxx] Geben Sie Ihren Ausdruck ein:
[20-xx-xxxx xx:xx:xx:xxx] Ergebnis: 4
```

3. **Debugging mit Logger**:

Für komplexeres Debugging fügen Sie `Logger.log(variable);` in Ihren Code ein, um den Zustand von Variablen zu drucken. Dies hilft Ihnen, den Fluss und die Zwischenzustände Ihrer Skripte zu verstehen.

## Tiefere Einblicke

Das Konzept eines REPL ist tief in der Geschichte der Computerwissenschaft verankert und stammt aus den Time-Sharing-Systemen der 1960er Jahre, die interaktive Sitzungen ermöglichten. Sprachen wie Lisp gediehen in dieser Umgebung, da das REPL für ihren iterativen Entwicklungsprozess entscheidend war. Im Gegensatz dazu, ist Google Apps Script, das wesentlich später entstanden ist, hauptsächlich für das Web konzipiert und konzentriert sich auf die Automatisierung von Aufgaben innerhalb der Google-Suite anstatt auf iterative, konsolenbasierte Programmierung.

Google Apps Script unterstützt traditionell keine Echtzeit-, interaktiven Codierungssitzungen sofort aus der Box aufgrund seiner Cloud-Natur und dem Fokus auf Web-App-Bereitstellung. Sein Ausführungsmodell dreht sich um Funktionen, die durch Web-Events, zeitgesteuerte Trigger oder manuelle Aufrufe innerhalb der Umgebung ausgelöst werden, und nicht um sofortige Feedback-Schleifen, die ein REPL bietet.

Obwohl das provisorische REPL und der Debugger im Apps Script Editor ein gewisses Maß an Interaktivität bieten, replizieren sie nicht vollständig das sofortige Feedback und die Effizienz traditioneller REPLs, die in vielen Programmiersprachen zu finden sind. Entwickler, die ein authentischeres REPL-Erlebnis mit Google-Technologien suchen, könnten externe JavaScript-Umgebungen oder Node.js mit Googles APIs erkunden. Diese können eine reaktionsfähigere und interaktivere Codierungssitzung bieten, erfordern allerdings mehr Einrichtung und führen möglicherweise aus dem direkten Apps Script-Umfeld heraus.
