---
title:                "Eine Datumsangabe in einen String umwandeln"
html_title:           "TypeScript: Eine Datumsangabe in einen String umwandeln"
simple_title:         "Eine Datumsangabe in einen String umwandeln"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn Du schon einmal mit Datumswerten in Deinem TypeScript-Code gearbeitet hast, bist Du vielleicht auf das Problem gestoßen, dass Du ein Datum in einen String umwandeln musst. Das kann aus verschiedenen Gründen notwendig sein, zum Beispiel für die Ausgabe auf einer Webseite oder für die Speicherung in einer Datenbank. Aber wie macht man das in TypeScript? In diesem Artikel erfährst Du es.

## Wie man ein Datum in einen String konvertiert

Um ein Datum in einen String zu konvertieren, gibt es in TypeScript die dafür vorgesehene `toString()` Methode. Das klingt vielleicht einfach, aber es gibt ein paar Dinge, die Du dabei beachten solltest. Schauen wir uns dazu ein Beispiel an:

```TypeScript
let date = new Date(); // Erstelle ein neues Datum-Objekt für den aktuellen Zeitpunkt
console.log(date.toString()); // Gibt den aktuellen Zeitpunkt im gängigen String-Format aus
```

Der obige Code erstellt ein neues Datum-Objekt unter Verwendung der `Date()` Klasse und gibt dann den aktuellen Zeitpunkt in einem String aus. In der Regel wird dabei das Datum in dem Format "Wochentag Monat Tag Jahr Stunden:Minuten:Sekunden Zeitzone" ausgegeben. Zum Beispiel "Tue Apr 13 2021 12:00:00 GMT+0200".

Aber was ist, wenn Du das Datum in einem bestimmten Format ausgeben möchtest? Dafür gibt es in TypeScript zusätzlich die `toLocaleDateString()` Methode, die es ermöglicht, das Datum in einem benutzerdefinierten Format auszugeben. Schauen wir uns dazu ein weiteres Beispiel an:

```TypeScript
let date = new Date(); // Erstelle ein neues Datum-Objekt für den aktuellen Zeitpunkt
console.log(date.toLocaleDateString('de-DE')); // Gibt das Datum in einem deutschen Format aus (z.B. "13.04.2021")
```

In diesem Beispiel verwenden wir die `toLocaleDateString()` Methode und übergeben als Parameter den Ländercode für Deutschland (de-DE). Dadurch wird das Datum in einem deutschsprachigen Format ausgegeben, zum Beispiel "13.04.2021".

## Tiefergehende Informationen

Wenn Du mehr über das Arbeiten mit Datumswerten in TypeScript erfahren möchtest, empfehle ich Dir die offizielle Dokumentation von TypeScript zu lesen. Dort findest Du weitere Details und Beispiele zu den verschiedenen Datums-Methoden und Formaten.

## Siehe auch

- [Offizielle TypeScript Dokumentation](https://www.typescriptlang.org/docs/handbook/working-with-dates)
- [Date-Objekt Referenz](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript Date Grundlagen](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Date)