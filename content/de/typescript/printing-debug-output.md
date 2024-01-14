---
title:                "TypeScript: Debug-Ausgabe drucken"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debuggen ist ein wichtiger Teil des Entwicklungsprozesses und kann dazu beitragen, Fehler in unserem Code zu finden und zu beheben. Das Drucken von Debug-Ausgaben kann uns dabei helfen, die Abfolge von Ereignissen in unserem Programm zu verstehen und Fehler leichter zu erkennen.

## Wie man Druckausgabe macht

Um Debug-Ausgaben in TypeScript zu machen, können wir die `console.log()` Funktion verwenden. Zum Beispiel:

```TypeScript
let num: number = 10;
console.log(num);
// Ausgabe: 10
```

Wir können auch mehrere Werte in einer Debug-Ausgabe drucken, indem wir sie durch Kommas trennen:

```TypeScript
let num1: number = 10;
let num2: number = 20;
console.log(num1, num2);
// Ausgabe: 10, 20
```

Eine weitere nützliche Funktion ist `console.debug()`, die speziell für Debug-Ausgaben verwendet wird. Sie kann uns helfen, Informationen über das Programm auszugeben, ohne die Konsolenausgabe zu überladen. Zum Beispiel:

```TypeScript
let num1: number = 10;
let num2: number = 20;
console.debug("Die Summe von", num1, "und", num2, "ist", num1 + num2);
// Ausgabe: DEBUG -> Die Summe von 10 und 20 ist 30
```

Wir können auch bedingte Debug-Ausgaben erstellen, die nur ausgeführt werden, wenn eine bestimmte Bedingung erfüllt ist. Hier ist ein Beispiel, in dem die Debug-Ausgabe nur gemacht wird, wenn `num > 10` ist:

```TypeScript
let num: number = 12;
if (num > 10) {
    console.log(num);
    // Ausgabe: 12
}
```

## Tiefergehende Informationen

Debug-Ausgaben können besonders hilfreich sein, wenn wir mit komplexen Datenstrukturen oder Funktionsaufrufen arbeiten. Wir können sie verwenden, um zu überprüfen, ob unsere Daten die erwarteten Werte haben oder um zu sehen, in welchem Teil unserer Funktion ein Fehler auftritt.

Außerdem können wir mithilfe von Debug-Ausgaben auch die Zeitdauer von bestimmten Operationen messen, um Engpässe in unserer Anwendung zu erkennen und zu optimieren.

Es ist jedoch wichtig zu beachten, dass Debug-Ausgaben nur für den Entwicklungsprozess gedacht sind und in der endgültigen Version unseres Codes entfernt werden sollten.

## Siehe auch

- [Console API Dokumentation (auf Englisch)](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [How To: Debugging in TypeScript (auf Englisch)](https://blog.logrocket.com/typescript-debugging/)
- [TypeScript: Debugging in VS Code (auf Englisch)](https://code.visualstudio.com/docs/typescript/typescript-debugging)