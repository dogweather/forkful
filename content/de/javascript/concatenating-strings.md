---
title:                "Javascript: Verketten von Zeichenketten"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Strängenkonkatenation ist ein wichtiges Konzept in der Javascript-Programmierung. Sie ermöglicht es uns, mehrere Strings miteinander zu verbinden und effizient große Textblöcke zu erstellen. Durch die Verwendung von Konkatenation können wir auch dynamische Texte erstellen, die sich je nach Bedingungen oder Benutzereingaben ändern können.

## Wie geht es

Um zwei Strings in Javascript zu verbinden, können wir den "+" Operator verwenden. Zum Beispiel:

```Javascript
var str1 = "Hallo";
var str2 = "Welt";
var combined = str1 + " " + str2;

console.log(combined);
// Output: "Hallo Welt"
```
Wir können auch mehr als zwei Strings verketten, indem wir den Operator mehrmals verwenden. Beachten Sie jedoch, dass die Reihenfolge der Strings in der Ausgabe der Reihenfolge entsprechen wird, in der sie verketten wurden.

```Javascript
var first = "Das";
var second = "ist";
var third= "eine";
var fourth = "Verkettung";
var sentence = first + " " + second + " " + third + " " + fourth;

console.log(sentence);
// Ausgabe: "Das ist eine Verkettung"
```

## Tiefergehende Einblicke

Beim Verketten von Strings in Javascript ist es wichtig, die Datentypen zu beachten. Wenn einer der zu verkettenen Werte kein String ist, wird dieser implizit in einen String konvertiert. Zum Beispiel:

```Javascript
// Automatische Konvertierung in Strings
var num = 2020;
var str = "Das Jahr ";
var result = str + num;

console.log(result);
// Output: "Das Jahr 2020"
```

Ein weiterer wichtiger Punkt ist die Verwendung von Backticks (`) anstelle von Anführungszeichen (") für Strings, die Variablen oder Ausdrücke enthalten. Dadurch können wir Platzhalter in unserem String verwenden, die beim Ausführen dynamisch ersetzt werden können. Zum Beispiel:

```Javascript
var name = "Maria";
var age = 25;

var message = `Hallo, mein Name ist ${name} und ich bin ${age} Jahre alt.`;

console.log(message);
// Ausgabe: "Hallo, mein Name ist Maria und ich bin 25 Jahre alt."
```

## Siehe auch

- [MDN Web Docs: Stringkonkatenation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String#Konkatenationen)
- [W3Schools: Javascript Zeichenketten](https://www.w3schools.com/js/js_strings.asp)