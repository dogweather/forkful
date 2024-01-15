---
title:                "Zwei Daten vergleichen"
html_title:           "TypeScript: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vergleichen von zwei Datumsangaben kann sehr nützlich sein, um beispielsweise zu überprüfen, ob ein bestimmter Zeitraum abgelaufen ist oder um zu bestimmen, welches Datum früher oder später ist.

## Anleitung

Um zwei Datumsangaben in TypeScript zu vergleichen, können wir die Methode `getTime()` verwenden. Diese Methode gibt die Anzahl der Millisekunden seit dem 1. Januar 1970 zurück. Wir können also einfach die beiden Datumsangaben in Millisekunden umwandeln und sie dann miteinander vergleichen.

```typescript
const date1 = new Date("2021-01-01");
const date2 = new Date("2020-01-01");

// Vergleichen ob date1 früher ist als date2
if (date1.getTime() < date2.getTime()) {
  console.log("date1 ist früher als date2");
}

// Vergleichen ob date1 später ist als date2
if (date1.getTime() > date2.getTime()) {
  console.log("date1 ist später als date2");
}

// Vergleichen ob beide Datumsangaben gleich sind
if (date1.getTime() === date2.getTime()) {
  console.log("beide Datumsangaben sind gleich");
}
```

Die Ausgabe dieses Beispiels ist:

```
date1 ist später als date2
```

## Tiefergehender Einblick

Es ist wichtig zu beachten, dass die `getTime()`-Methode nicht das eigentliche Datum vergleicht, sondern nur die Anzahl der Millisekunden seit dem 1. Januar 1970. Das bedeutet, dass zwei unterschiedliche Datumsangaben, die eigentlich gleich sind, möglicherweise nicht gleich sein werden, wenn sie in Millisekunden umgewandelt werden. Zum Beispiel:

```typescript
const date1 = new Date("2021-01-01");
const date2 = new Date("2021-01-01");

// Beide Datumsangaben sind gleich, aber...
if (date1.getTime() === date2.getTime()) {
  console.log("beide Datumsangaben sind gleich");
}

// ...das hier wird nicht ausgeführt, weil die Millisekunden abweichen
if (date1 === date2) {
  console.log("beide Datumsangaben sind gleich");
}
```

Die Ausgabe ist hier nur:

```
beide Datumsangaben sind gleich
```

Außerdem müssen wir auch berücksichtigen, dass die `getTime()`-Methode das Datum in Bezug auf die Zeitzone des Browsers zurückgibt. Wenn wir also auf verschiedenen Geräten mit unterschiedlichen Zeitzonen arbeiten, kann es zu unterschiedlichen Ergebnissen beim Vergleichen der Datumsangaben kommen. Um dies zu vermeiden, können wir die `getTimezoneOffset()`-Methode verwenden, um den Zeitunterschied zu berücksichtigen.

## Siehe auch

- [MDN Web Docs: Methode `getTime()`](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime)
- [MDN Web Docs: Methode `getTimezoneOffset()`](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/getTimezoneOffset)
- [W3Schools: Vergleichen von Datumsangaben in TypeScript](https://www.w3schools.com/js/js_date_methods.asp)