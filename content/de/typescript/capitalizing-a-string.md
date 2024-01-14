---
title:                "TypeScript: Ein String großschreiben"
simple_title:         "Ein String großschreiben"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Warum: Es ist oft notwendig, einen Satz zu groß zu schreiben, sei es für einheitliches Formatieren oder um hervorzuheben, dass es sich um einen Titel oder eine Überschrift handelt.

Wie es geht: Wenn Sie eine Zeichenkette in TypeScript programmieren, gibt es verschiedene Möglichkeiten, sie zu groß zu schreiben. Eine Möglichkeit besteht darin, ein benutzerdefiniertes Funktionen zu schreiben, die jeden Buchstaben in der Zeichenkette in Großbuchstaben umwandelt. Eine andere Möglichkeit ist die Verwendung der built-in Methode "toUpperCase()", die in vielen Programmiersprachen verfügbar ist.

```TypeScript
// Benutzerdefinierte Funktion
function capitalizeString(str: string): string {
  let result = "";
  for (let i=0; i<str.length; i++) {
    // Überprüfen, ob der Buchstabe ein Kleinbuchstabe ist
    if (str[i] >= 'a' && str[i] <= 'z') {
      // Wandelt in Großbuchstaben um und fügt den Buchstaben dem Ergebnis hinzu
      result += str[i].toUpperCase();
    } else {
      // Fügt den Buchstaben ohne Veränderung dem Ergebnis hinzu
      result += str[i];
    }
  }
  return result;
}

let input = "Dies ist ein Test";
let output = capitalizeString(input);
console.log(output); // "DIES IST EIN TEST"

// Verwendung der built-in Methode toUpperCase()
let input = "Dies ist ein Test";
let output = input.toUpperCase();
console.log(output); // "DIES IST EIN TEST"
```

Tief einsteigen: Es gibt viele Faktoren, die bei der Groß- und Kleinschreibung von Zeichenketten in verschiedenen Programmiersprachen eine Rolle spielen. Beispielsweise kann die Sprache oder das Zeichenset des Systems Einfluss darauf haben, wie Buchstaben transformiert werden. Es ist wichtig, sich die Dokumentation der verwendeten Programmiersprache anzusehen, um sicherzustellen, dass die Funktion ordnungsgemäß funktioniert und keine unerwarteten Ergebnisse liefert.

Siehe auch:
- [toUpperCase() Methode](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Reguläre Ausdrücke in TypeScript](https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm)
- [So formatieren Sie eine gesamte Zeichenkette in TypeScript](https://stackoverflow.com/questions/35635232/how-to-reformat-a-whole-string-in-typescript)