---
title:                "Erfassen des aktuellen Datums"
html_title:           "TypeScript: Erfassen des aktuellen Datums"
simple_title:         "Erfassen des aktuellen Datums"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum ist eine wichtige Information in vielen Programmen, sei es für die Anzeige auf einer Benutzeroberfläche, als Teil eines Zeitstempels oder für komplexe Datumsberechnungen. Im TypeScript gibt es verschiedene Möglichkeiten, das aktuelle Datum zu erhalten und zu manipulieren. In diesem Artikel werfen wir einen kurzen Blick darauf, wie man das aktuelle Datum in TypeScript erhalten kann.

## Wie geht man vor?

Um das aktuelle Datum in TypeScript zu erhalten, können wir die integrierte `Date()`-Funktion verwenden. Diese Funktion gibt ein Objekt mit dem Datum und der Zeit des aktuellen Zeitpunkts zurück. Wir können dann Methoden wie `getDate()` und `getMonth()` verwenden, um bestimmte Teile des Datums zu erhalten.

```TypeScript
let currentDate = new Date();
console.log(currentDate); // Ausgabe: current Date and time object

let currentDay = currentDate.getDate();
console.log(currentDay); // Ausgabe: aktueller Tag
```

Um das Datum in einem bestimmten Format auszugeben, können wir die `toLocaleDateString()`-Methode verwenden. Diese Methode akzeptiert als Parameter das gewünschte Format, in dem das Datum angezeigt werden soll.

```TypeScript
let currentDate = new Date();
console.log(currentDate.toLocaleDateString("de-DE")); // Ausgabe: 09.12.2021 (Dezember 09, 2021)
```

Es gibt auch die Möglichkeit, das aktuelle Datum in einer bestimmten Zeitzone zu erhalten. Dazu können wir die `toLocaleTimeString()`-Methode verwenden und als Parameter eine Zeitzone übergeben, z.B. "en-US" für die Zeitzone "Eastern Standard Time" in den USA.

```TypeScript
let currentDate = new Date();
console.log(currentDate.toLocaleTimeString("en-US")); // Ausgabe: 11:29:00 PM (Eastern Standard Time)
```

## Tiefergehende Einblicke

Das aktuelle Datum kann in TypeScript auch durch die Verwendung von Bibliotheken wie `moment.js` oder `date-fns` verarbeitet werden. Diese Bibliotheken bieten viele nützliche Funktionen für die Arbeit mit Datumsangaben und erleichtern die Datumsberechnungen.

Es ist auch wichtig zu beachten, dass das Datum von JavaScript-Engines aufgrund von Zeitzoneunterschieden und Schaltsekunden möglicherweise nicht immer präzise ist. Deshalb ist es ratsam, mit diesen Unterschieden bei der Verarbeitung von Datumswerten umzugehen.

## Siehe auch

- [Offizielle TypeScript-Dokumentation zu Date-Objekten](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Moment.js-Library-Dokumentation](https://momentjs.com/docs/)
- [Date-fns-Library-Dokumentation](https://date-fns.org/docs/Getting-Started)