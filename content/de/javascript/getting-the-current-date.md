---
title:                "Das aktuelle Datum erhalten"
html_title:           "Javascript: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums und der aktuellen Uhrzeit kann hilfreich sein, um dynamische Funktionen in einer Anwendung zu integrieren oder um Daten zu organisieren und zu sortieren. Es ist auch eine grundlegende Fertigkeit, die bei der Arbeit mit Javascript nützlich ist.

## Wie geht's?

Die zeitliche Funktion in Javascript ist `Date()`, die sowohl Datum als auch Uhrzeit zurückgibt. Um das aktuelle Datum und die aktuelle Uhrzeit zu erhalten, können wir einfach `new Date()` verwenden. Lassen Sie uns das in Aktion sehen:

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

Dies würde das aktuelle Datum und die aktuelle Uhrzeit in Standard-Format zurückgeben, z.B.: `Sun Jan 10 2021 11:50:13 GMT-0500 (Eastern Standard Time)`

Wenn Sie das Datum in einem bestimmten Format benötigen, gibt es verschiedene Methoden, die in Kombination mit `Date()` verwendet werden können. Hier sind einige Beispiele:

- `getDate()` gibt den Tag des Monats zurück (1-31)
- `getMonth()` gibt den Monat zurück (0-11, wobei 0 für Januar steht)
- `getFullYear()` gibt das Jahr zurück (z.B. 2021)
- `getHours()` gibt die Stunden zurück (0-23)
- `getMinutes()` gibt die Minuten zurück (0-59)
- `getSeconds()` gibt die Sekunden zurück (0-59)

Lassen Sie uns einige dieser Funktionen verwenden, um das Datum und die Uhrzeit in einem benutzerdefinierten Format auszugeben:

```Javascript
let currentDate = new Date();
let dayOfMonth = currentDate.getDate();
let month = currentDate.getMonth() + 1; // Wir fügen 1 hinzu, um den Monat mit dem richtigen Wert (1-12) anzuzeigen
let year = currentDate.getFullYear();
let hours = currentDate.getHours();
let minutes = currentDate.getMinutes();
let seconds = currentDate.getSeconds();

console.log(`${dayOfMonth}/${month}/${year} ${hours}:${minutes}:${seconds}`);
```

Dies würde ein Datum und eine Uhrzeit im Format `10/1/2021 11:55:23` ausgeben.

## Tiefer tauchen

Es gibt viele weitere Methoden und Optionen, um das Datum in Javascript zu formatieren. Sie können z.B. auch die `toLocaleDateString()`-Funktion verwenden, um das Datum in einer bestimmten Sprache und einem bestimmten Format zurückzugeben. Eine vollständige Liste der verfügbaren Methoden für das `Date`-Objekt finden Sie in der [offiziellen Dokumentation von MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date).

Sie können auch Module oder Bibliotheken wie [moment.js](https://momentjs.com/) verwenden, um das Verarbeiten und Formatieren von Datum und Uhrzeit in Javascript zu vereinfachen.

## Siehe auch

- [MDN - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Get Current Date](https://www.w3schools.com/js/js_date_methods.asp)
- [The Ultimate Guide to Date in Javascript](https://www.smashingmagazine.com/2020/04/working-with-dates-moment-timezone-date-fns-luxon/) (auf Englisch)