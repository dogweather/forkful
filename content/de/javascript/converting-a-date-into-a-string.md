---
title:                "Javascript: Eine Datum in eine Zeichenfolge umwandeln."
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von einem Date-Objekt in einen String ist eine wichtige Funktion in der JavaScript Programmierung, da es es ermöglicht, Daten in einem für den Menschen lesbares Format darzustellen. Dies kann besonders hilfreich sein, wenn man mit Datum- und Zeitangaben arbeitet und diese in einer ansprechenden Form präsentieren möchte.

## Wie man ein Date-Objekt in einen String umwandelt

Weiter unten finden Sie ein Beispiel, das zeigt, wie man ein Date-Objekt in einen String umwandelt. Es gibt mehrere Möglichkeiten, dies zu tun, aber im Allgemeinen wird dafür die Methode `toDateString()` verwendet. Diese Methode gibt ein Standard-Datums-Format zurück, das je nach Region unterschiedlich sein kann.

```Javascript
let today = new Date();
let dateString = today.toDateString();
console.log(dateString);
```

Dieses Beispiel erzeugt die folgende Ausgabe:

```Javascript
Wed Nov 24 2021
```

Man kann diese Methode auch mit anderen Methoden kombinieren, um verschiedene Datums- und Zeitformate zu erhalten. Zum Beispiel könnte man die Methode `toLocaleDateString()` verwenden, um das Datum in der jeweiligen Lokalzeitzone darzustellen, oder `toTimeString()` für die Uhrzeit alleine.

Hier ist ein Beispiel, das das Datum und die Zeit im amerikanischen Format darstellt:

```Javascript
let now = new Date();
let dateString = now.toLocaleDateString('en-US', {month: '2-digit', day: '2-digit', year: 'numeric'});
let timeString = now.toLocaleTimeString('en-US');
console.log(`${dateString}, ${timeString}`);
```

Diese Code-Abschnitte ergeben zusammen folgende Ausgabe:

```Javascript
11/24/2021, 9:30:15 PM
```

## Tiefer in die Konvertierung von Dateien in Strings eintauchen

Beim Konvertieren von Date-Objekten in Strings gibt es ein paar Dinge zu beachten. Zum einen gibt es verschiedene Methoden, um das Datum in unterschiedlichen Formaten darzustellen. Außerdem sollte man bedenken, dass die Darstellung des Datums von der Region oder den Ländereinstellungen des Geräts abhängen kann.

Eine weitere Überlegung ist die Handhabung von Zeitzonen. Wenn Sie ein Datum aus einer bestimmten Zeitzone in eine andere konvertieren, müssen Sie möglicherweise eine Zeitzonenanpassung vornehmen, um ein korrektes Ergebnis zu erhalten.

Es ist ebenfalls wichtig zu wissen, dass JavaScript-Date-Objekte keine eingebauten Formatierungsfunktionen haben. Dies bedeutet, dass man selbst entscheiden muss, wie man das Datum und die Uhrzeit anzeigen möchte.

In der [offiziellen JavaScript Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date) finden Sie weitere Informationen zu Date-Objekten und deren Verwendung.

## Siehe auch

- [Die offizielle JavaScript Dokumentation zu Date-Objekten](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date-Objekte in Strings konvertieren - ein Tutorial von w3schools](https://www.w3schools.com/js/js_dates_string.asp)
- [JavaScript Date Object Umwandlung in UTC und DateStrings - ein Beitrag von Flavio Copes](https://flaviocopes.com/how-to-convert-date-to-string-javascript/)