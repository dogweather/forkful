---
title:                "Ein Datum aus einem String analysieren"
html_title:           "Javascript: Ein Datum aus einem String analysieren"
simple_title:         "Ein Datum aus einem String analysieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String ist ein häufig verwendeter Prozess in der Programmierung. Es bezieht sich auf die Konvertierung eines Datums, das in einem lesbaren Format wie z.B. "05.12.2020" in einen Datentyp umgewandelt wird, den das Programm verarbeiten kann.

Programmierer verwenden das Parsen von Datumsangaben aus Strings, um die Eingabe von Benutzern in ihr Programm zu interpretieren. So können Benutzer beispielsweise ein Datum in einem Eingabefeld im Format "TT.MM.JJJJ" eingeben, und das Programm wird das richtige Datum verstehen und darauf basierend weiterarbeiten.

## Wie?
Um ein Datum aus einem String zu parsen, gibt es verschiedene Möglichkeiten in Javascript. Eine Möglichkeit ist die Verwendung der integrierten Date-Methode `Date.parse()`. Hier ein Beispiel, wie man dies in Code umsetzen würde:
```Javascript
let dateStr = "05.12.2020";
let parsedDate = Date.parse(dateStr);
console.log(parsedDate);
```

Ausgabe: 1607136000000 (entspricht dem Datum in Millisekunden seit dem 01.01.1970)

Eine andere Möglichkeit ist die Verwendung der `Date()`-Konstruktorfunktion. Hier ein Beispiel:
```Javascript
let dateStr = "05.12.2020";
let dateArray = dateStr.split("."); // Datumstring nach "." aufteilen
let parsedDate = new Date(dateArray[2], dateArray[1] - 1, dateArray[0]); // Datum mit Array-Werten erstellen
console.log(parsedDate);
```

Ausgabe: Sa Dec 05 2020 00:00: 00 GMT+0100 (Mitteleuropäische Normalzeit)

## Tiefer eintauchen
Das Parsen von Datumsangaben aus Strings ist eine häufige Aufgabe in der Programmierung und wurde bereits in frühen Programmiersprachen wie COBOL verwendet. Es gibt auch andere Möglichkeiten, Datumswerte in Javascript zu verarbeiten, z.B. mit Hilfe von Bibliotheken wie Moment.js oder mit dem `toLocaleDateString()`-Methode.

Die `Date.parse()`-Methode kann auch eine ISO-Datumsangabe (z.B. "2020-12-05") verarbeiten und gibt das Datum als Millisekunden seit dem 01.01.1970 zurück.

## Siehe auch
- [Date.parse() Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [Moment.js Bibliothek](https://momentjs.com/)
- [`toLocaleDateString()` Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)