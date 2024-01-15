---
title:                "Ausgabe von Debug-Meldungen"
html_title:           "TypeScript: Ausgabe von Debug-Meldungen"
simple_title:         "Ausgabe von Debug-Meldungen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man Debug-Ausgaben drucken? Nun, Debugging ist ein wichtiger Teil des Programmierens und kann dabei helfen, Fehler und Probleme in unserem Code zu identifizieren und zu beheben. Das Drucken von Debug-Ausgaben ist eine Möglichkeit, den Ablauf unseres Codes zu verfolgen und eventuelle Fehler zu erkennen.

## Wie geht das?

In TypeScript gibt es mehrere Möglichkeiten, Debug-Ausgaben zu drucken. Eine einfache Möglichkeit ist die Verwendung von `console.log()` Befehlen. Diese können verwendet werden, um Variablen, Objekte oder einfach nur Text zu drucken.

```TypeScript
let name = "Max Mustermann";
console.log(name); // Ausgabe: Max Mustermann
```

Eine weitere Möglichkeit ist die Verwendung von der `debugger` Anweisung. Diese stoppt die Ausführung unseres Codes und öffnet die Entwicklerkonsole, wo wir die Werte unserer Variablen überprüfen und Fehler finden können.

```TypeScript
let num1 = 5;
let num2 = 10;
debugger; // Code wird hier angehalten
let sum = num1 + num2;
console.log(sum); // Ausgabe: 15
```

Wenn wir mehrere Variablen oder komplexe Objekte ausgeben möchten, können wir auch die `JSON.stringify()` Methode verwenden, um sie in einen lesbaren String umzuwandeln.

```TypeScript
let person = {
  name: "Lisa",
  age: 25,
  hobbies: ["lesen", "malen", "reisen"]
};
console.log(JSON.stringify(person));

// Ausgabe: {"name":"Lisa","age":25,"hobbies":["lesen","malen","reisen"]}
```

## Tiefer in die Materie

Debugging ist oft ein zeitaufwendiger Prozess, und Debug-Ausgaben können uns dabei helfen, diesen Prozess zu beschleunigen und effizienter zu gestalten. Indem wir gezielt Debug-Ausgaben in unserem Code platzieren, können wir bestimmte Bereiche isolieren und überprüfen, wo möglicherweise ein Fehler auftritt.

Eine nützliche Technik ist auch das Hinzufügen von Bedingungen zu unseren `console.log()` Befehlen, um sie nur unter bestimmten Bedingungen auszuführen. Zum Beispiel könnten wir nur dann eine Debug-Ausgabe drucken, wenn eine bestimmte Variable einen bestimmten Wert hat.

Eine weitere Möglichkeit ist die Verwendung von speziellen Debugging-Tools wie dem TypeScript-Debugger in Visual Studio Code. Dieses Tool bietet eine Vielzahl an Funktionen, um unser Debugging effizienter zu gestalten, wie zum Beispiel das Setzen von Breakpoints, Überwachen von Variablen und Schritt-für-Schritt-Durchlauf des Codes.

## Siehe auch

- [Offizielle TypeScript-Website](https://www.typescriptlang.org/)
- [Visual Studio Code](https://code.visualstudio.com/) (IDE für TypeScript-Entwicklung)
- [Debugging in TypeScript](https://code.visualstudio.com/docs/typescript/typescript-debugging)