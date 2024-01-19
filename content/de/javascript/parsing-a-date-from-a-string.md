---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Datums-Formatierung in Javascript

## Was & Warum?

Die Analyse (Parsing) eines Datums aus einem String in Javascript ermöglicht es uns, Text in ein Datum umzuwandeln. Das brauchen wir, um flexible Datenmanipulationen und -operationen zu ermöglichen.

## So geht's:

In Javascript verwenden wir das `Date` Objekt und seine Methode `.parse()`, um ein Datum aus einem String zu erstellen.

```Javascript
let datumString = "2023-07-25T10:15:00Z";
let datum = new Date(datumString);
console.log(datum);
```

Wenn Sie das ausführen, wird das erhaltene Ergebnis etwa so aussehen:
```Javascript
Tue Jul 25 2023 12:15:00 GMT+0200 (Mitteleuropäische Sommerzeit)
```

## Tiefergrund

Ursprünglich konnte Javascript nur englische Datumsangaben analysieren. Mit ECMAScript 5 wurde jedoch ein ISO 8601 Format eingeführt, das jetzt standardmäßig verwendet wird.

Es gibt auch Bibliotheken wie Moment.js, die mehr Formate und Funktionen anbieten, aber sie fügen Ihrer Anwendung zusätzliche Größe hinzu.

Die Javascript Datumsanalyse erfolgt in der lokalen Zeitzone des Computers - es sei denn, es wird, wie im obigen Beispiel, das 'Z'-Zeitzone-Format verwendet.

## Siehe auch

- [MDN Dokumentation für Date.parse()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [ISO 8601 Datum und Zeit Format](https://www.cl.cam.ac.uk/~mgk25/iso-time.html)
- [Moment.js](http://momentjs.com/)