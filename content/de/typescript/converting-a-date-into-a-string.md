---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?

Umwandeln eines Datums in einen String (Date to String) ermöglicht uns, das Datum in eine gut lesbare, menschenfreundliche Form zu präsentieren. Programmierer machen das, um ein konsistentes und lokalisiertes Format zu gewährleisten, das leicht zu verstehen ist.

# Wie macht man das?

Die `toLocaleString()` Funktion kann verwendet werden, um ein Datum in einen String umzuwandeln. Schauen wir uns ein Beispiel an:

```TypeScript
let aktuellesDatum: Date = new Date();
let datumAlsString: string = aktuellesDatum.toLocaleString('de-DE');
console.log(datumAlsString);
```

In der Ausgabe könnte etwas wie "26.5.2021, 12:30:45" erscheinen, was ein für Deutschsprachige interpretierbares Datum ist.

## Tiefentauchen

Das Konzept des String-Rendering von Daten kommt aus den frühen Tagen der Informatik, als Terminals ausschließlich Text darstellten. Die Umsetzung in TypeScript basiert auf ECMAScript internationalization API, die verschiedene Kulturen unterstützt.

Es gibt auch Alternativmethoden wie `toISOString()` oder `toDateString()`. Ihre Verwendung hängt vom gewünschten Format ab.

Zudem ist die genaue Implementierung von `toLocaleString()` browserabhängig, sodass die Ausgabe leicht variieren kann.

## Siehe auch

- [MDN: Date.prototype.toLocaleString()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [TC39: Proposal Temporal](https://github.com/tc39/proposal-temporal), eine moderne Alternative zu `Date`.