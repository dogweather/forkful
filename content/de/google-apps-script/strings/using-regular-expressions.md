---
title:                "Reguläre Ausdrücke verwenden"
aliases: - /de/google-apps-script/using-regular-expressions.md
date:                  2024-02-01T22:04:32.744356-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguläre Ausdrücke verwenden"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/using-regular-expressions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (regex) sind Muster, die verwendet werden, um Zeichenkombinationen in Zeichenketten zu finden. Programmierer nutzen sie zum Suchen, Bearbeiten oder Manipulieren von Text und Daten, was sie unverzichtbar für Mustererkennung und Datenverarbeitungsaufgaben macht.

## Wie:

Die Verwendung regulärer Ausdrücke in Google Apps Script ist dank der JavaScript-basierten Syntax unkompliziert. Hier ist, wie Sie regex in Ihren Skripten für gängige Aufgaben wie Suchen und Datenvalidierung einbauen können.

### Zeichenketten durchsuchen

Angenommen, Sie möchten herausfinden, ob eine Zeichenkette ein bestimmtes Muster enthält, wie z.B. eine E-Mail-Adresse. Hier ist ein einfaches Beispiel:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Gefunden: " + found[0]);
  } else {
    Logger.log("Keine E-Mail gefunden.");
  }
}

// Beispielverwendung
findEmailInText("Kontaktieren Sie uns unter info@example.com.");
```

### Datenvalidierung

Reguläre Ausdrücke glänzen bei der Datenvalidierung. Unten ist eine Funktion, die eine Eingabezeichenkette validiert, um zu überprüfen, ob sie einer einfachen Passwortrichtlinie entspricht (mindestens ein Großbuchstabe, ein Kleinbuchstabe und mindestens 8 Zeichen).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Beispiel-Ausgabe
Logger.log(validatePassword("Str0ngPass")); // Gibt aus: true
Logger.log(validatePassword("schwach"));    // Gibt aus: false
```

## Vertiefung

Reguläre Ausdrücke in Google Apps Script werden von JavaScript geerbt, das erstmals im Juni 1997 in der ECMAScript-Sprachspezifikation standardisiert wurde. Obwohl mächtig, können sie manchmal zu verwirrendem und schwer zu wartendem Code führen, insbesondere wenn sie übermäßig oder für komplexe Mustererkennungsaufgaben verwendet werden, die möglicherweise effizienter durch andere Parsing-Methoden gelöst werden könnten.

Beispielsweise ist die Verwendung von Regex für das Parsing von HTML oder XML im Notfall möglich, wird jedoch generell aufgrund der verschachtelten und komplizierten Strukturen dieser Dokumente abgeraten. Stattdessen sind Werkzeuge, die speziell für das Parsing solcher Strukturen entwickelt wurden, wie DOM-Parser für HTML, zuverlässiger und lesbarer.

Darüber hinaus sollten Google Apps Script-Entwickler auf potenzielle Leistungsprobleme achten, wenn sie komplexe Regex-Muster in großangelegten Textmanipulationsaufgaben verwenden, da die Regex-Verarbeitung CPU-intensiv sein kann. In solchen Fällen könnte das Aufteilen der Aufgabe in einfachere Teil-Aufgaben oder die Verwendung integrierter String-Manipulationsfunktionen einen besseren Ausgleich zwischen Leistung und Wartbarkeit bieten.
