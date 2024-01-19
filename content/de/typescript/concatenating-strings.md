---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was und Warum?
Die Zeichenkettenverknüpfung – oder einfach 'Concatenation' genannt – ist der Prozess, bei dem zwei oder mehr Zeichenketten zu einer einzigen verbunden werden. Programmierer machen es typischerweise, um die Lesbarkeit zu verbessern und redundante Codezeilen zu reduzieren.

## Anleitung
In TypeScript gibt es mehrere Möglichkeiten, Zeichenketten zu verknüpfen. Hier sind ein paar Beispiele:

```TypeScript
// Methode 1: Mit dem Pluszeichen (+)
let string1 = "Hallo, ";
let string2 = "Welt!";
let verknüpfteString = string1 + string2;
console.log(verknüpfteString); // Gibt "Hallo, Welt!" aus
 
// Methode 2: Mit der Methode concat()
let verknüpfteString2 = string1.concat(string2);
console.log(verknüpfteString2); // Gibt auch "Hallo, Welt!" aus

// Methode 3: Mit Template Literal (Backtick-Syntax)
let verknüpfteString3 = `${string1}${string2}`;
console.log(verknüpfteString3); // Gibt ebenfalls "Hallo, Welt!" aus
```

## Tiefere Einblicke
Historisch gesehen verwenden JavaScript (und infolgedessen TypeScript) das '+' als hauptsächliche Methode zur Zeichenkettenverknüpfung, aber mit zunehmender Evolution des Sprachstandards sind Template Literale und die `concat()` Methode hinzugefügt worden.

Template Literale (eingeführt in ES6) sind besonders nützlich, wenn Sie Variablen innerhalb von Zeichenketten einfügen möchten, ohne lästige '+' Operatoren zu verwenden. Die Methode `concat()`, obwohl weniger verbreitet, kann nützlich sein, wenn Sie mehrere Zeichenketten in einer Operation verknüpfen möchten.

Beachten Sie, dass alle oben genannten Methoden eine neue Zeichenkette zurückgeben, anstatt die ursprüngliche Zeichenkette zu verändern. Dies liegt daran, dass Zeichenketten in JavaScript und TypeScript unveränderlich sind.

## Siehe Auch
- [MDN Web Docs: String.prototype.concat()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN Web Docs: Template Strings](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript Handbook: Basic Types – String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)