---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Konvertierung eines Datums in einen String ist eine Verfahren, in dem wir ein Datum in lesbare Textform bringen. Dies ermöglicht eine einfachere Darstellung und Handhabung von Daten in menschenlesenbare Formate.

## So wird's gemacht:

```Javascript
let aktuellesDatum = new Date();
let datumString = aktuellesDatum.toString();

console.log(datumString);
```

Die Ausgabe könnte so aussehen:

```Javascript
"Tue Mar 30 2021 14:20:58 GMT+0200 (Mitteleuropäische Sommerzeit)"
```

Ebenso können wir das Datum nach gewünschtem Format konvertieren:

```Javascript
let aktuellesDatum = new Date();
let datumString = aktuellesDatum.toDateString();

console.log(datumString);
```

Die Ausgabe könnte so aussehen:

```Javascript
"Tue Mar 30 2021"
```

## Tiefere Eintauchen

Historisch gesehen, begann die Vorstellung von Zeichenketten von Daten mit der Einführung von Hochsprachen, die menschenlesbare Codes verwenden. Ursprünglich haben Maschinensprachen nur mit Zahlen gearbeitet.

Alternativen zur `toString()` Methode sind `toDateString()`, `toLocaleString()`, `toLocaleDateString()` und viele mehr, je nachdem welche Information und Format benötigt wird.

Die Implementierung von Datum zu String-Konvertierung ist eine eingebaute JavaScript-Funktion und nicht in den Kern der Sprache eingebettet. Sie ruft einfach nur die Standard-Bibliotheksfunktion auf und gibt das Ergebnis zurück.

## Siehe auch

Weitere verwandte Ressourcen können unter diesen Links gefunden werden:

1. MDN Web Docs: [Date.prototype.toString()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
2. JavaScript.info: [Date and time](https://javascript.info/date)
3. Stack Overflow: [Mini-debate on toString()](https://stackoverflow.com/questions/3552461/how-to-format-a-javascript-date)