---
title:    "TypeScript: Das aktuelle Datum erhalten"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist eine häufige Notwendigkeit bei der Programmierung. Es kann verwendet werden, um ein Datumsfeld in einem Formular vorzubelegen, um die Erstellung von Zeitstempeln zu automatisieren oder um die letzte Aktualisierung einer Datei oder Datenbank anzuzeigen. Es ist also eine nützliche Funktion, um zu wissen, wie man sie in TypeScript umsetzt.

## Wie geht man vor

Es gibt mehrere Möglichkeiten, um das aktuelle Datum in TypeScript abzurufen. Eine Möglichkeit ist die Verwendung der integrierten JavaScript-Dateiobjekte. Zum Beispiel kann man das "Date" Objekt verwenden, um das aktuelle Datum und die aktuelle Zeit abzurufen. 

```TypeScript
let currentDate = new Date(); 
console.log(currentDate); 
```
 Die Konsole würde dann das aktuelle Datum und die aktuelle Uhrzeit in der folgenden Form ausgeben:
 ```TypeScript
 Sun Jun 20 2021 10:37:50 GMT-0400 (Eastern Daylight Time)
 ```

 Eine andere Möglichkeit ist die Verwendung von Bibliotheken wie Moment.js oder date-fns, die zahlreiche Funktionen zum Umgang mit Datum und Zeit bieten. Zum Beispiel könnte man Moment.js verwenden, um das aktuelle Datum als ein bestimmtes Format anzuzeigen:

 ```TypeScript
 import moment from 'moment';
 let currentDate = moment().format('MMMM Do YYYY, h:mm:ss a');
 console.log(currentDate);
 ```
 Die Konsole würde dann das aktuelle Datum und die aktuelle Uhrzeit in der folgenden Form ausgeben:
 ```TypeScript
 June 20th 2021, 10:37:50 am
 ```

 Wenn man das aktuelle Datum in einem HTML-Formular vorbefüllen möchte, könnte man dies wie folgt tun:

 ```TypeScript
 let dateInput = document.getElementById("datum") as HTMLInputElement;
 dateInput.value = new Date().toISOString().split("T")[0];
 ```
 Dies würde das aktuelle Datum im ISO-Format als Wert für ein Eingabefeld mit der ID "datum" einfügen.

## Tiefere Einblicke

Es gibt einige Dinge, die man beachten sollte, wenn man das aktuelle Datum in TypeScript verwendet. Zum Beispiel gibt es Unterschiede in den Zeit- und Datumsformaten, je nachdem, in welchem Land oder in welcher Region man sich befindet. Dies kann bei der Formatierung und Ausgabe des Datums berücksichtigt werden.

Außerdem kann es sinnvoll sein, das Datum und die Zeit in UTC zu erhalten, um mögliche Zeitzonenprobleme zu vermeiden. Dazu kann man die entsprechende Methode für UTC verwenden, wie zum Beispiel `getUTCDate()` oder `getUTCHours()`. 

Es kann auch hilfreich sein, sich mit den verschiedenen Funktionen und Eigenschaften des Date-Objekts vertraut zu machen, um das Datum und die Zeit weiter zu verarbeiten.

## Siehe auch

- [Date Objekt - MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - Dokumentation](https://momentjs.com/docs/)
- [date-fns - Dokumentation](https://date-fns.org/docs/)