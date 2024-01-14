---
title:    "TypeScript: Eine Datum in eine Zeichenfolge umwandeln"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von einem Datum in einen String kann hilfreich sein, um zum Beispiel Datumsangaben in einer anderen Form zu präsentieren oder für die Datenspeicherung in einer Datenbank.

## Wie

Das Konvertieren eines Datums in einen String ist in TypeScript relativ einfach. Hier ist ein Beispiel einer Funktion, die ein Date-Objekt als Parameter erhält und eine formatierte Zeichenkette zurückgibt:

```TypeScript
function dateToString(date: Date): string {
  const day = date.getDate();
  const month = date.getMonth() + 1;
  const year = date.getFullYear();
  return `${day}/${month}/${year}`;
}

const currentDate = new Date();
const dateString = dateToString(currentDate);

// Output: "30/04/2021"
```

## Deep Dive

Beim Konvertieren eines Datums in einen String gibt es einige Dinge zu beachten. Zunächst muss das Date-Objekt in seine Bestandteile (Tag, Monat und Jahr) zerlegt werden, da es sich um numerische Werte handelt. Dann können diese Werte mithilfe von String Interpolation in das gewünschte Format gebracht werden. In diesem Beispiel haben wir das Datum im Format "dd/mm/yyyy" ausgegeben, aber je nach Bedarf können auch andere Formate verwendet werden.

Ein weiterer wichtiger Faktor ist die Sprache der Ausgabe. Standardmäßig gibt die `getDate()`-Methode den Tag des Monats zurück, während `getMonth()` den Monat als Zahl von 0 bis 11 zurückgibt. Es ist also wichtig zu beachten, dass die Monatszahl um eins erhöht werden muss, bevor sie in die Ausgabe eingefügt wird.

## Siehe auch

- [Formatieren von Daten in TypeScript](https://www.typescriptlang.org/docs/handbook/destructuring.html)
- [Date-Objekt in TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)