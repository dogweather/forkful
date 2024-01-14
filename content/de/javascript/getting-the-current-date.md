---
title:                "Javascript: Das aktuelle Datum erhalten"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Viele Anwendungen erfordern es, das aktuelle Datum und die aktuelle Uhrzeit auszugeben. Es kann für eine bestimmte Funktionalität, wie zum Beispiel eine Erinnerungsfunktion, benötigt werden, oder einfach nur als Information für den Nutzer angezeigt werden. In diesem Blog-Beitrag werden wir uns anschauen, wie wir mithilfe von Javascript das aktuelle Datum auf einfache Weise erhalten können.

## Wie es geht

Die aktuelle Datum und Uhrzeit können in Javascript mit der `Date()` Methode abgerufen werden. Wir können sie in einer Variablen speichern, um sie später zu verwenden oder direkt als Ausgabe nutzen.

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

Die Ausgabe wird in der Konsole folgendermaßen aussehen:

```
Fri Jul 23 2021 17:50:46 GMT+0200 (Central European Summer Time)
```

Dies gibt uns das aktuelle Datum, die Uhrzeit und die Zeitzone an. Aber was ist, wenn wir nur das Datum oder die Uhrzeit benötigen?

Um nur das aktuelle Datum zu erhalten, können wir die `getDate()` Methode verwenden, die uns den Tag des Monats als Zahl zurückgibt:

```Javascript
let currentDate = new Date();
let day = currentDate.getDate();
console.log(day);
```

Dies gibt uns als Ausgabe den aktuellen Tag des Monats, zum Beispiel 23. Um zusätzlich den Monat und das Jahr zu bekommen, können wir die `getMonth()` und `getFullYear()` Methoden verwenden:

```Javascript
let currentDate = new Date();
let month = currentDate.getMonth() + 1; // Die Monate starten bei 0, daher muss 1 addiert werden
let year = currentDate.getFullYear();
console.log(day + "." + month + "." + year);
```

Die Ausgabe wird nun wie folgt aussehen:

```
23.7.2021
```

Um nur die aktuelle Uhrzeit zu erhalten, können wir die `getHours()`, `getMinutes()` und `getSeconds()` Methoden nutzen:

```Javascript
let currentDate = new Date();
let hours = currentDate.getHours();
let minutes = currentDate.getMinutes();
let seconds = currentDate.getSeconds();
console.log(hours + ":" + minutes + ":" + seconds);
```

Dies wird uns die aktuelle Uhrzeit im Format Stunden:Minuten:Sekunden zurückgegeben.

## Tiefentauchgang

Wenn wir uns die Ausgabe des `console.log()` genauer ansehen, fällt auf, dass es sich um einen sogenannten Timestamp handelt. Dies ist eine Zahl, die angibt, wie viele Millisekunden seit dem 1. Januar 1970 vergangen sind. Wir können diesen Timestamp auch direkt ausgeben, indem wir die `getTime()` Methode verwenden:

```Javascript
let currentDate = new Date();
let timestamp = currentDate.getTime();
console.log(timestamp);
```

Die Ausgabe wird nun eine lange Zahl sein, zum Beispiel 1627051101584. Dies kann nützlich sein, um verschiedene Zeitberechnungen durchzuführen.

## Siehe auch

- [MDN Web Docs - Date()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript Date() Method](https://www.w3schools.com/jsref/jsref_date.asp)
- [Date and Time in Javascript](https://www.toptal.com/software/definitive-guide-to-datetime-manipulation)