---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:01.795763-07:00
description: "Das Verketten von Zeichenketten beinhaltet das Kombinieren von zwei\
  \ oder mehreren Zeichenketten zu einer einzelnen Zeichenkette. Programmierer machen\u2026"
lastmod: '2024-03-13T22:44:53.325181-06:00'
model: gpt-4-0125-preview
summary: "Das Verketten von Zeichenketten beinhaltet das Kombinieren von zwei oder\
  \ mehreren Zeichenketten zu einer einzelnen Zeichenkette. Programmierer machen\u2026"
title: Strings verketten
---

{{< edit_this_page >}}

## Was & Warum?

Das Verketten von Zeichenketten beinhaltet das Kombinieren von zwei oder mehreren Zeichenketten zu einer einzelnen Zeichenkette. Programmierer machen dies, um Nachrichten, URLs oder jede Form von Text, der eine Mischung aus statischem und variablem Inhalt erfordert, dynamisch zu erstellen.

## Wie:

In Google Apps Script, das auf JavaScript basiert, gibt es mehrere Möglichkeiten, Zeichenketten zu verketten. Hier sind einige gängige Methoden:

### Verwendung des Plusoperators (`+`):

```javascript
var firstName = "John";
var lastName = "Doe";
var fullName = firstName + " " + lastName;
Logger.log(fullName); // Ausgabe: John Doe
```

### Nutzung der `concat()`-Methode:

```javascript
var string1 = "Hallo";
var string2 = "Welt";
var combinedString = string1.concat(" ", string2);
Logger.log(combinedString); // Ausgabe: Hallo Welt
```

### Verwendung von Template-Literalen (Backticks):

Dies ist eine moderne und flexible Art, Zeichenketten zu verketten, die es Ihnen ermöglicht, Ausdrücke leicht innerhalb von Zeichenketten einzubetten.

```javascript
var language = "Google Apps Script";
var message = `Das Lernen von ${language} macht Spaß!`;
Logger.log(message); // Ausgabe: Das Lernen von Google Apps Script macht Spaß!
```

Jede dieser Methoden hat ihre Anwendungsfälle und die Wahl zwischen ihnen hängt typischerweise von den Anforderungen an die Lesbarkeit und der Komplexität der zu verbindenden Zeichenketten ab.

## Tiefergehend

Das Verketten von Zeichenketten ist ein fundamentaler Aspekt, nicht nur in Google Apps Script, sondern in vielen Programmiersprachen. Historisch wurde das Verketten von Zeichenketten oft mit dem Plusoperator oder spezialisierten Funktionen/Methoden wie `concat()` durchgeführt. Jedoch, mit der Einführung von Template-Literalen in ECMAScript 2015 (ES6), die Google Apps Script unterstützt, haben Entwickler eine leistungsfähigere und intuitivere Art und Weise erhalten, mit Zeichenketten umzugehen.

Template-Literale vereinfachen nicht nur die Syntax für das Einbetten von Ausdrücken in Zeichenketten, sondern unterstützen auch mehrzeilige Zeichenketten ohne die Notwendigkeit expliziter Zeilenumbruchzeichen. Dies verringert das Potenzial für Fehler und verbessert die Lesbarkeit des Codes, insbesondere beim Umgang mit komplexen Zeichenketten oder beim Einsetzen mehrerer Variablen in eine Textvorlage.

Während der `+` Operator und die `concat()`-Methode weiterhin weit verbreitet und unterstützt werden für die Rückwärtskompatibilität und Einfachheit in einfacheren Szenarien, bieten Template-Literale eine moderne, ausdrucksstarke Alternative, die oft für die Zeichenkettenverkettung als überlegen angesehen wird, besonders wenn Lesbarkeit und Wartbarkeit von Bedeutung sind.

Es ist dennoch wichtig, die Methode zu wählen, die am besten zum spezifischen Kontext und den Anforderungen Ihres Projekts passt, wobei Faktoren wie die Kompatibilität der Zielumgebung (obwohl dies selten ein Problem bei Google Apps Script darstellt), die Auswirkungen auf die Leistung (minimal für die meisten Anwendungen) und die Vertrautheit des Entwicklungsteams mit modernen JavaScript-Funktionen berücksichtigt werden.
