---
title:                "Eine Datum in eine Zeichenfolge umwandeln"
html_title:           "Javascript: Eine Datum in eine Zeichenfolge umwandeln"
simple_title:         "Eine Datum in eine Zeichenfolge umwandeln"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man ein Datum in eine Zeichenfolge (String) konvertieren möchte. Eine häufige Anwendung ist die Darstellung von Datumswerten in einer bestimmten Formatierung, zum Beispiel in einem Benutzerinterface oder in einem Bericht.

## Wie geht's

Die Konvertierung von einem Datum in eine Zeichenfolge kann in Javascript auf verschiedene Weisen erreicht werden, abhängig von der gewünschten Formatierung.

Die einfachste Möglichkeit ist die Verwendung der "toString()" Methode. Diese Methode wird auf ein Datum-Objekt aufgerufen und gibt eine Standard-Zeichenfolge in folgendem Format zurück: "Wochentag Monat Tag Jahr Stunden:Minuten:Sekunden Zeitzone".

```javascript
const date = new Date(); // aktuelles Datum

const dateString = date.toString(); // "Wed Jun 16 2021 09:30:00 GMT+0200 (Central European Summer Time)"
```

Möchte man hingegen ein Datum in einem benutzerdefinierten Format ausgeben, kann die "toLocaleDateString()" Methode verwendet werden. Diese Methode akzeptiert als Parameter die gewünschte Sprache und Optionen für die Formatierung.

```javascript
const date = new Date(); // aktuelles Datum

const options = { year: 'numeric', month: 'short', day: 'numeric', timeZone: 'UTC' };

const dateString = date.toLocaleDateString('de-DE', options); // "16. Jun 2021"
```

Alternativ gibt es die Möglichkeit, das "Intl" Objekt zu nutzen, um ein Datum in verschiedenen Sprachen und Formatierungen auszugeben. Hierzu muss jedoch das "Intl" Objekt im Browser oder Node.js aktiviert sein.

```javascript
const date = new Date(2021, 5, 16); // 16. Juni 2021

const germanDate = new Intl.DateTimeFormat('de-DE', { dateStyle: 'full' }).format(date); // "Mittwoch, 16. Juni 2021"
const frenchDate = new Intl.DateTimeFormat('fr-FR', { weekday: 'short', year: 'numeric', month: 'long' }).format(date); // "mer. 16 juin 2021"
```

## Tiefer schauen

Wenn man sich genauer mit der Konvertierung von Datum in Zeichenfolge befassen möchte, gibt es einige wichtige Punkte zu beachten.

Zunächst ist es wichtig zu wissen, dass der Ausgangspunkt für ein Datum in Javascript der 1. Januar 1970 00:00:00 UTC (Koordinierte Weltzeit) ist. Datumswerte werden als Millisekunden seit diesem Zeitpunkt gespeichert. Daher muss bei der Konvertierung immer eine Zeitzone berücksichtigt werden, um ein korrektes Ergebnis zu erhalten.

Ein weiterer wichtiger Faktor ist die internationale Formatierung von Datum und Uhrzeit. Da die Darstellung von Datumswerten je nach Region und Kultur unterschiedlich sein kann, ist es sinnvoll, sich mit den verschiedenen Optionen und Methoden zur Formatierung vertraut zu machen.

## Siehe auch

- [MDN Dokumentation zu Date.toString()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [MDN Dokumentation zu Date.toLocaleDateString()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [MDN Dokumentation zum Intl Objekt](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Intl)