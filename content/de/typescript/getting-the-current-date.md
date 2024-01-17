---
title:                "Das heutige Datum erhalten"
html_title:           "TypeScript: Das heutige Datum erhalten"
simple_title:         "Das heutige Datum erhalten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist eine häufige Aufgabe in der Programmierung. Es ermöglicht Programmierern, genau zu bestimmen, wann ein bestimmtes Ereignis stattgefunden hat oder wann ein bestimmter Abschnitt des Codes ausgeführt wurde.

## Wie geht es?
Die aktuelle Datum Funktion wird in TypeScript durch die Verwendung des Date-Objekts durchgeführt. Hier ist ein Beispiel, das das Datum und die Uhrzeit des heutigen Tages ausgibt:

```TypeScript
let today = new Date ();
console.log (today);
```

Dieser Code wird ein Objekt erstellen, das das aktuelle Datum und die Uhrzeit im folgenden Format enthält: " Monat Tag Jahr Stunden: Minuten: Sekunden ". Zum Beispiel: "May 2 2021 12:01:37".

Sie können auch spezifische Informationen des aktuellen Datums wie den Monat, Tag oder die Stunden abrufen. Hier ist ein Beispiel, das den aktuellen Monat ausgibt:

```TypeScript
let currentMonth = today.getMonth ();
console.log (currentMonth);
```

Dies wird das aktuelle Monat als Zahl ausgeben, wobei Januar als 0 und Dezember als 11 dargestellt werden.

## Tief tauchen
Die Verwendung des Date-Objekts in TypeScript ist Teil des ECMAScript-Standards und wurde 1995 in JavaScript eingeführt. Es gibt auch alternative Möglichkeiten, das aktuelle Datum in TypeScript abzurufen, wie z.B. durch die Verwendung von Drittanbieter-Bibliotheken oder durch die Verwendung von eingebauten Funktionen wie "new Date.now ()".

Um das Datum in einem bestimmten Format auszugeben, können Sie auch die Methoden des Date-Objekts wie "toDateString ()" oder "toLocaleDateString ()" verwenden.

## Siehe auch
Weitere Informationen über den Date-Objekt und die verschiedenen Methoden, um das aktuelle Datum in TypeScript abzurufen, können in der offiziellen TypeScript-Dokumentation gefunden werden: https://www.typescriptlang.org/docs/handbook/standard-library.html#date

Für zusätzliche Alternativen oder detaillierte Informationen über die Geschichte des Date-Objekts, können Sie auch die folgenden Ressourcen überprüfen:

- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- https://www.w3schools.com/jsref/jsref_date.asp