---
title:                "Zwei Daten vergleichen"
html_title:           "Javascript: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Hast du dich jemals gefragt, wie man zwei Daten in Javascript vergleichen kann? Es gibt viele Anwendungsfälle, wie zum Beispiel die Überprüfung, ob eine eingestellte Frist erreicht wurde oder das Sortieren von Daten in einem bestimmten Zeitraum.

## Wie geht das?

```Javascript
let date1 = new Date('2021-01-01');
let date2 = new Date('2021-01-05');

// Vergleiche ob date1 vor date2 liegt
if (date1 < date2) {
  console.log("date1 liegt vor date2");
}

// Vergleiche ob date1 nach date2 liegt
if (date1 > date2) {
  console.log("date1 liegt nach date2");
}

// Vergleiche ob date1 und date2 gleich sind
if (date1.getTime() === date2.getTime()) {
  console.log("date1 und date2 sind gleich");
}

```

Die Vergleichsoperatoren `<` und `>` können verwendet werden, um zu überprüfen, welches Datum früher oder später liegt. Aber um sicherzustellen, dass die beiden Daten auch wirklich gleich sind, müssen wir die `getTime()`-Methode verwenden, die die Zeit in Millisekunden seit dem 1. Januar 1970 zurückgibt.

## Tiefer Einblick

Ein weiterer wichtiger Aspekt beim Vergleichen von Daten ist die Zeitzone. Wenn wir zum Beispiel ein Datum aus einer anderen Zeitzone erhalten und es mit einem lokalen Datum vergleichen, kann dies zu unerwarteten Ergebnissen führen. In diesem Fall müssen wir die `getTimezoneOffset()`-Methode verwenden, die die Zeitdifferenz zwischen der lokalen Zeitzone und der UTC-Zeitzone zurückgibt.

```Javascript
let date1 = new Date('2021-01-01T00:00:00-08:00'); // Datum aus PST-Zeitzone
let date2 = new Date('2021-01-01T00:00:00Z'); // Datum aus UTC-Zeitzone

if (date1.getTime() === date2.getTime() + date2.getTimezoneOffset() * 60000) {
  console.log("date1 und date2 sind gleich");
}

```

Hier wird die Zeitzoneoffset-Differenz von `date2` in Millisekunden umgewandelt und zum Zeitwert von `date2` addiert, bevor sie mit dem Zeitwert von `date1` verglichen wird.

## Siehe auch

- [MDN Dokumentation über die Date-Klasse in Javascript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Bibliothek für das Arbeiten mit Datum und Zeit](https://momentjs.com/)
- [Vergleichsoperatoren in Javascript](https://www.w3schools.com/js/js_comparisons.asp)