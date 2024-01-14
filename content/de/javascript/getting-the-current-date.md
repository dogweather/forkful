---
title:    "Javascript: Das aktuelle Datum erhalten"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Zeit ist die Verwendung von Datumsangaben in der Programmierung unerlässlich. Ob es darum geht, ein Datum für einen Kalender oder eine Uhr anzuzeigen, oder um zu überprüfen, ob ein bestimmtes Ereignis bereits stattgefunden hat - das aktuelle Datum ist ein wichtiges Element bei der Arbeit mit Javascript. In diesem Beitrag zeigen wir Ihnen, wie Sie das aktuelle Datum in Ihrem Javascript-Code abrufen können.

## Wie geht das?

Die Verwendung des aktuellen Datums in Javascript ist eigentlich ziemlich einfach. Alles, was Sie tun müssen, ist die integrierte `Date()` Funktion aufzurufen, ohne ihr Parameter zu übergeben. Hier ist ein Beispiel:

```Javascript
var currentDate = new Date();

console.log(currentDate);
```

Dieses kurze Stück Code wird das aktuelle Datum und die aktuelle Uhrzeit in der Konsole ausgeben, wie zum Beispiel `Wed Aug 04 2021 09:49:34 GMT+0200 (Mitteleuropäische Sommerzeit)`.

Sie können auch bestimmte Informationen aus dem `Date`-Objekt extrahieren, wie zum Beispiel den Tag, den Monat oder das Jahr. Hier ist ein Beispiel, das das aktuelle Jahr ausgibt:

```Javascript
var currentYear = new Date().getFullYear();

console.log(currentYear);
```

Dieser Code wird einfach die aktuelle Jahreszahl ausgeben, ohne den Rest der Datumswerte.

## Tiefentauchen

Hinter den Kulissen führt die `Date()`-Funktion eine ganze Reihe von Operationen aus, um das aktuelle Datum und die aktuelle Uhrzeit abzurufen. Sie nutzt den internen Zeitgeber des Computers, um die aktuelle Uhrzeit in Millisekunden seit dem 1. Januar 1970 zu ermitteln, einem wichtigen Datum in der Programmierung, das als der Nullpunkt der Zeitmessung gilt. Dann werden diese Millisekunden in ein menschenlesbares Datum und eine Uhrzeit umgewandelt, indem sie in Tage, Monate, Jahre usw. aufgeteilt werden.

Es ist wichtig zu beachten, dass die Zeitzone des Computers das Ergebnis beeinflussen kann. Um sicherzustellen, dass Sie das korrekte Datum und die korrekte Uhrzeit abrufen, sollten Sie die Zeitzone in Ihrem Code angeben.

## Siehe auch

* [MDN Web Docs - Datum und Uhrzeit in Javascript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date) 
* [W3Schools - Das Javascript-`Date`-Objekt](https://www.w3schools.com/jsref/jsref_obj_date.asp)
* [StackOverflow - Javascript-Datum in bestimmter Zeitzone erhalten](https://stackoverflow.com/questions/1091372/how-do-i-get-the-time-of-day-in-javascript)