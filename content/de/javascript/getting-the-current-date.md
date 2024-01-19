---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Javascript aktuelles Datum und Zeit holen
## Was und Warum?
Das Abrufen des aktuellen Datums in JavaScript bietet uns ein live Zeitstempel. Programmierer verwenden es, um Aktivitäten zu protokollieren, Zeitberechnungen durchzuführen oder Ereignisse zu planen.

## So geht's:
Hier ist ein einfacher Weg, um das aktuelle Datum und die aktuelle Uhrzeit in JavaScript zu erhalten.

```Javascript
let jetzt = new Date();
console.log(jetzt);
```

Wenn Sie diesen Code ausführen, erhalten Sie eine Ausgabe ähnlich dieser:

```
Wed Oct 20 2021 16:34:22 GMT+0200 (Mitteleuropäische Sommerzeit)
```

## Vertiefung
Historisch gesehen verwendet JavaScript das Date-Objekt und seine Methoden, um mit Datums- und Zeitwerten zu arbeiten. Wir haben Alternativen wie die `moment.js` Bibliothek für komplexere Anforderungen.

Darüber hinaus sollte beachtet werden, dass JavaScript das Datum und die Uhrzeit im Browser des Benutzers basierend auf dessen Zeitzoneneinstellungen ermittelt. Dies bedeutet, dass der Wert, den Sie erhalten, von Benutzer zu Benutzer unterschiedlich sein kann.

## Siehe Auch:
1. [MDN Web Docs: Date](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date): Weiterführende Informationen über das Date-Objekt in JavaScript.
2. [Moment.js](https://momentjs.com/): Eine leistungsfähige Bibliothek zum Arbeiten mit Datum und Uhrzeit in JavaScript.