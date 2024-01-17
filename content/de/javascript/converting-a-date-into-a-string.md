---
title:                "Das Umwandeln eines Datums in einen String"
html_title:           "Javascript: Das Umwandeln eines Datums in einen String"
simple_title:         "Das Umwandeln eines Datums in einen String"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Was und warum?
Das Konvertieren eines Datums in einen String bedeutet, ein bestimmtes Datum in eine lesbare und formatierte Zeichenfolge zu verwandeln. Programmierer verwenden dies, um Informationen über ein bestimmtes Datum anzuzeigen oder zu speichern.

Wie geht das?
Das Konvertieren eines Datums in einen String ist eine einfache Aufgabe in Javascript. Hier sind zwei Beispiele, wie man das machen kann:

```Javascript
// Beispiel 1: Konvertieren in ein standardisiertes Datumsformat
const date = new Date();
const stringDate = date.toDateString();
console.log(stringDate); // Ausgabe: "Fri Feb 19 2021"

// Beispiel 2: Konvertieren in ein benutzerdefiniertes Datumsformat
const date = new Date();
const stringDate = date.toLocaleDateString('de-DE', { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' });
console.log(stringDate); // Ausgabe: "Freitag, 19. Februar 2021"
```

Tiefgehende Informationen:
In der Vergangenheit war das Konvertieren von Datumswerten in Strings eine komplexere Aufgabe, da es keine einheitlichen Datenformate gab und die Programme in verschiedenen Sprachen geschrieben wurden. Alternativen zur Umwandlung von Datumswerten in Strings sind das Arbeiten mit Datumsobjekten oder das Verwenden von Bibliotheken wie Moment.js. In der Implementierung des Beispiels 2 wird die Methode `toLocaleDateString` verwendet, die abhängig von der Browsersprache unterschiedliche Ergebnisse liefert.

Siehe auch:
- [MDN Web Docs - Date.prototype.toDateString()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
- [MDN Web Docs - Date.prototype.toLocaleDateString()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)