---
title:                "Javascript: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit ist ein häufiges Szenario in der Programmierung. Es kann verwendet werden, um Deadlines einzustellen, Timer für Benachrichtigungen zu programmieren oder einfach nur um zu zeigen, dass man coole Datumsberechnungen durchführen kann.

## Wie Geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, gibt es verschiedene Wege in Javascript. Eine Möglichkeit ist die Verwendung der "Date" -Funktion, die es uns ermöglicht, ein Datum anzugeben und verschiedene Methoden wie "setFullYear ()" oder "setDate ()" zu verwenden, um Änderungen vorzunehmen.

```Javascript 
// Berechnung eines Datums in der Zukunft
let heute = new Date();
let anzahlTage = 7;
heute.setDate(heute.getDate() + anzahlTage);
console.log("Datum in 7 Tagen: " + heute);
// Ausgabe: Datum in 7 Tagen: Tue Sep 15 2020 18:05:13 GMT+0200 (Central European Summer Time)
```

Eine andere Möglichkeit ist die Verwendung von Bibliotheken wie "moment.js", die speziell für die Arbeit mit Datum und Uhrzeit entwickelt wurden. Diese Bibliothek bietet viele nützliche Funktionen wie das Hinzufügen oder Subtrahieren von Tagen, Monaten oder Jahren von einem gegebenen Datum.

```Javascript 
// Datum in der Zukunft mit moment.js berechnen
let heute = moment();
let anzahlTage = 7;
heute.add(anzahlTage, 'days');
console.log("Datum in 7 Tagen: " + heute);
// Ausgabe: Datum in 7 Tagen: 2020-09-15T18:05:13+02:00
```

## Tiefere Einblicke

Beim Berechnen von Daten in Zukunft oder Vergangenheit müssen wir möglicherweise auch auf Dinge wie Schaltjahre oder unterschiedliche Zeitzonen achten. Es ist wichtig zu verstehen, wie die Methode "getDate ()" funktioniert und wie man sie für unsere spezifischen Berechnungen anpassen kann.

Außerdem können wir mit moment.js auch präzise Ergebnisse erhalten, indem wir die Funktion "locale" nutzen, um auf die Zeitzone und Spracheinstellungen des Benutzers zuzugreifen.

## Siehe Auch

- Dokumentation zur "Date" -Funktion in Javascript: https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date
- Dokumentation zu moment.js: https://momentjs.com/docs/