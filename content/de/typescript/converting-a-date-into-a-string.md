---
title:                "Eine Datum in eine Zeichenfolge umwandeln."
html_title:           "TypeScript: Eine Datum in eine Zeichenfolge umwandeln."
simple_title:         "Eine Datum in eine Zeichenfolge umwandeln."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Konvertieren eines Datums in eine Zeichenkette ist ein gängiger Prozess in der Programmierung, bei dem ein Datumsobjekt in ein einfacheres Format umgewandelt wird, das leichter lesbar und verarbeitet werden kann. Programmierer verwenden dieses Verfahren beispielsweise, wenn sie Datumsangaben in Textnachrichten oder Dateinamen einbetten möchten.

## So geht's:
```TypeScript
// Datum in eine Zeichenkette mit dem Standardformat konvertieren
let dateToString = new Date().toISOString();

// Ausgabe: 2020-10-05T14:30:00.000Z

// Datum in eine Zeichenkette mit benutzerdefiniertem Format konvertieren
let options = { year: 'numeric', month: 'short', day: 'numeric' };
let dateToStringCustom = new Date().toLocaleDateString('de-DE', options);

// Ausgabe: 5. Oktober 2020
```

## Tiefere Einblicke:
Das Konzept, ein Datum in eine Zeichenkette umzuwandeln, ist nicht neu und wurde bereits in frühen Programmiersprachen wie BASIC verwendet. In TypeScript gibt es mehrere Methoden, um dieses Verfahren durchzuführen, einschließlich der Verwendung von integrierten Funktionen wie `toISOString()` und `toLocaleDateString()`, aber auch der Verwendung von externen Bibliotheken wie Moment.js. Es ist auch möglich, benutzerdefinierte Formate zu erstellen, um das Datum in verschiedenen Sprachen und für verschiedene Anwendungsfälle darzustellen.

## Siehe auch:
- [MDN Web Docs: Zusammenarbeit mit Datum und Uhrzeit](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Dokumentation](https://momentjs.com/docs/)
- [Codepen Beispiel: Konvertieren eines Datums in eine Zeichenkette](https://codepen.io/patrickkempff/pen/ZJqdmz)