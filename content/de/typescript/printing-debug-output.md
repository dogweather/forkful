---
title:    "TypeScript: Ausgabe von Debug-Informationen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Manchmal kann es schwierig sein, Fehler in unserem Code zu finden und zu verstehen, was genau falsch läuft. Hier kommen die sogenannten "Debug-Ausgaben" ins Spiel. Durch das gezielte Ausgeben von Informationen während der Laufzeit unseres Programms können wir besser nachvollziehen, was passiert und somit Fehler identifizieren und beheben. 

## So geht's

Um Debug-Ausgaben in TypeScript zu machen, können wir die Funktion `console.log()` verwenden. Diese Funktion gibt die übergebenen Werte in der Konsole unseres Browsers oder Terminals aus. Hier ist ein Beispiel:

```TypeScript
let num1 = 5;
let num2 = 10;
console.log("Die Summe von " + num1 + " und " + num2 + " ist " + (num1 + num2));

// Output: Die Summe von 5 und 10 ist 15
```

Wir können auch Objekte oder Arrays als Parameter an `console.log()` übergeben, um auch deren Inhalte auszugeben.
```TypeScript
let person = { name: "Max", alter: 26 };
console.log(person);

// Output: { name: "Max", alter: 26 }
```

### Tipp: Conditional Debugging
Manchmal möchten wir Debug-Ausgaben nur unter bestimmten Umständen sehen, zum Beispiel nur wenn eine Bedingung erfüllt ist. Dafür können wir `console.log()` in eine `if`-Anweisung einbinden:
```TypeScript
let num3 = 15;
if (num3 > 10) {
  console.log("Die Zahl ist größer als 10.");
}

// Output: Die Zahl ist größer als 10.
```

## Tiefer gehen

Neben der einfachen Verwendung von `console.log()` gibt es noch weitere Methoden, um Debug-Ausgaben zu machen. Dazu gehört zum Beispiel `console.error()`, um Fehlermeldungen auszugeben, oder `console.table()`, um Tabellen auszugeben. Eine vollständige Liste aller Methoden findet ihr in der offiziellen [Dokumentation von `console`](https://developer.mozilla.org/de/docs/Web/API/Console). 

Eine weitere nützliche Methode ist `console.assert()`, mit der wir überprüfen können, ob eine Bedingung wahr oder falsch ist und ggf. eine Fehlermeldung ausgeben können. Hier ist ein Beispiel:
```TypeScript
let num4 = 8;
console.assert(num4 % 2 === 0, "Die Zahl ist keine gerade Zahl.");

// Output: Assertion failed: Die Zahl ist keine gerade Zahl.
```

## Sieh auch

- [Dokumentation von `console`](https://developer.mozilla.org/de/docs/Web/API/Console)
- [Video Tutorial: Debugging in TypeScript](https://www.youtube.com/watch?v=HhUJkw7-hmQ)
- [Artikel: 5 Debugging-Tipps für TypeScript](https://www.sitepoint.com/debug-typescript/)