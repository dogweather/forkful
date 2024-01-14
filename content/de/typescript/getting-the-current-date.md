---
title:    "TypeScript: Das aktuelle Datum abrufen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist ein Grundkonzept, das in vielen TypeScript-Anwendungen benötigt wird. Es ermöglicht die Verwendung von Echtzeitinformationen in verschiedenen Funktionen und Anweisungen.

## Wie

Um das aktuelle Datum in TypeScript zu erhalten, können Sie die Date-Klasse und die getFullYear(), getMonth(), getDate(), getHours(), getMinutes() und getSeconds() Methoden verwenden. Diese Methoden geben jeweils eine Zahl zurück, die das entsprechende Datum oder die entsprechende Uhrzeit darstellt.

```TypeScript
let currentDate = new Date();

console.log(currentDate.getFullYear()); // gibt das aktuelle Jahr zurück
console.log(currentDate.getMonth()); // gibt den aktuellen Monat zurück (beginnend bei 0 für Januar)
console.log(currentDate.getDate()); // gibt den aktuellen Tag des Monats zurück
console.log(currentDate.getHours()); // gibt die aktuelle Stunde zurück (im 24-Stunden-Format)
console.log(currentDate.getMinutes()); // gibt die aktuellen Minuten zurück
console.log(currentDate.getSeconds()); // gibt die aktuellen Sekunden zurück
```

Die obigen Beispiele geben folgende Ausgabe zurück (basierend auf dem Datum, an dem dieser Code ausgeführt wurde):

```
2019
8 (September)
24
9
43
12
```

Es ist auch möglich, das aktuelle Datum und die Uhrzeit in einer bestimmten Zeitzone abzurufen, indem Sie die getTimezoneOffset() Methode verwenden und diese zu dem Datum hinzufügen:

```TypeScript
let currentDate = new Date();

console.log(currentDate.getTimezoneOffset()); // gibt den Zeitunterschied in Minuten zwischen lokaler Zeit und UTC-Zeit zurück
console.log(currentDate.getHours() + currentDate.getTimezoneOffset() / 60); // gibt die aktuelle Stunde basierend auf UTC-Zeit zurück
```

## Deep Dive

Die Date-Klasse in TypeScript basiert auf der Date-Klasse in JavaScript und hat ähnliche Methoden und Eigenschaften. Es gibt jedoch einige Unterschiede in der Art und Weise, wie ein Datum in TypeScript gehandhabt wird.

Zum Beispiel kann eine Instanz der Date-Klasse in TypeScript auf verschiedene Weise erstellt werden, einschließlich der Verwendung von Zahlen, die sowohl das Datum als auch die Uhrzeit darstellen, oder der Verwendung von Strings in verschiedenen Formaten, um ein bestimmtes Datum wiederzugeben.

Darüber hinaus ist es wichtig zu beachten, dass die Date-Klasse in TypeScript nicht für die Verwendung in nativem Code vorgesehen ist. Stattdessen handelt es sich um eine Entwicklungs-Hilfsklasse, die hauptsächlich für die Verwendung in Browsern und anderen Plattformen gedacht ist, die JavaScript unterstützen.

## Siehe auch

- [Das offizielle TypeScript-Handbuch](https://www.typescriptlang.org/docs/home.html)
- [Mozilla Developer Network: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) (Englisch)
- [TypeScript-Tutorial: Datum](https://www.tutorialspoint.com/typescript/typescript_date.htm)