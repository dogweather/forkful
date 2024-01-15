---
title:                "Suchen und Ersetzen von Text"
html_title:           "TypeScript: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Manchmal kommt es vor, dass wir in unserem Code Textpassagen ändern wollen. Sei es, um Fehler zu beheben oder um unsere Codebasis zu aktualisieren. Hier kommt die Funktion "Suchen und Ersetzen" ins Spiel, die uns dabei unterstützt, schnell und effizient Text in unserem Code zu finden und zu ersetzen. In diesem Artikel werden wir uns ansehen, wie wir diese Funktion in TypeScript verwenden können.

## Wie geht's

Die Suche und Ersetzen Funktion kann in TypeScript auf verschiedene Arten genutzt werden. Die einfachste Möglichkeit ist die Verwendung der integrierten Suchfunktion in den meisten Code Editoren. Wir können auch die String-Methode "replace()" oder die Regular Expression (Regex) verwenden, um Text zu finden und zu ersetzen.

### Mit dem integrierten Suchfeld

Das integrierte Suchfeld ermöglicht es uns, in unserem Code nach einem bestimmten Text zu suchen und ihn dann manuell zu ersetzen. Wir können auch die Option "Alle ersetzen" verwenden, um alle Vorkommnisse des gesuchten Textes auf einmal zu ersetzen.

```TypeScript
// In diesem Beispiel ersetzen wir "Hallo" mit "Hi" in einem String
let text = "Hallo, wie geht es dir?";
let newText = text.replace("Hallo", "Hi");
console.log(newText); // Ausgabe: "Hi, wie geht es dir?"
```

### Mit der replace() Methode

Die replace() Methode ermöglicht es uns, einen Text in einem String durch einen anderen zu ersetzen. Sie unterscheidet sich von der integrierten Suchfunktion darin, dass sie alle Vorkommnisse des gesuchten Textes standardmäßig ersetzt.

```TypeScript
// In diesem Beispiel ersetzen wir alle Vorkommnisse von "e" mit "i"
let text = "Elefant";
let newText = text.replace(/e/g, "i");
console.log(newText); // Ausgabe: "ilifanti"
```

### Mit Regular Expressions (Regex)

Regex ist eine leistungsstarke Möglichkeit, Text basierend auf bestimmten Mustern zu finden und zu ersetzen. Hier können wir unsere Suchanfragen noch genauer definieren.

```TypeScript
// In diesem Beispiel ersetzen wir alle Zahlen mit dem Text "Zahl"
let text = "Es gibt 3 Äpfel und 5 Bananen.";
let regex = /\d/g; // Das "g" bedeutet, dass alle Vorkommnisse ersetzt werden sollen
let newText = text.replace(regex, "Zahl");
console.log(newText); // Ausgabe: "Es gibt Zahl Äpfel und Zahl Bananen."
```

## Deep Dive

Die String-Methode "replace()" und die Verwendung von Regular Expressions können sehr mächtig sein, um Text in unserem Code zu ersetzen. Es ist jedoch wichtig zu beachten, dass sie auch unerwünschte Ergebnisse liefern können, wenn nicht sorgfältig verwendet. Eine falsch definierte Regular Expression kann zum Beispiel mehr Text ersetzen als beabsichtigt oder sogar ungewollte Textpassagen löschen.

Ein weiterer wichtiger Punkt ist die Verwendung von optionalen Parametern wie "i" für die Groß- und Kleinschreibung und "g" für alle Vorkommnisse. Es ist wichtig, die gewünschten Ergebnisse im Blick zu behalten und die entsprechenden Parameter zu setzen.

## Siehe auch

- [Mozilla Developer Network: String.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Mozilla Developer Network: Regular Expressions](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [VS Code Docs: Suchen und Ersetzen](https://code.visualstudio.com/docs/editor/codebasics#_search-and-replace)