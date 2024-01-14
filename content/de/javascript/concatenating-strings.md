---
title:                "Javascript: Verketten von Zeichenfolgen"
simple_title:         "Verketten von Zeichenfolgen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum?

Wenn du schon einmal Javascript programmiert hast, dann hast du mit hoher Wahrscheinlichkeit schon die Konkatenation von Strings verwendet. Aber warum ist das überhaupt wichtig? Die Antwort ist einfach: Die Konkatenation von Strings ermöglicht es dir, mehrere Texte oder Variablen zu einem zusammenzufügen und somit eine dynamische Ausgabe zu generieren.

## Wie geht's?

Um Strings in Javascript zu konkatieren, gibt es verschiedene Möglichkeiten. Die einfachste Methode ist die Verwendung des Plus-Zeichens `+`. Schau dir das folgende Beispiel an:

```Javascript
let name = "Max";
let message = "Hallo " + name + ", wie geht's?";
```

In diesem Beispiel nutzen wir das Plus-Zeichen, um den String `name` zur Nachricht hinzuzufügen. Das Ergebnis wäre dann: "Hallo Max, wie geht's?". Man kann auch mehrere Variablen oder Texte miteinander verknüpfen, indem man einfach weitere Plus-Zeichen hinzufügt.

Eine weitere Möglichkeit ist die Verwendung der `concat()` Methode. Schau dir dazu das folgende Beispiel an:

```Javascript
let name = "Anna";
let message = "Guten Tag ";
message = message.concat(name, ", wie geht es dir?");
```

In diesem Beispiel nutzen wir die `concat()` Methode, um den String `name` zur Nachricht hinzuzufügen. Das Ergebnis wäre hier ebenfalls: "Guten Tag Anna, wie geht es dir?".

## Tiefere Einblicke

Interessanterweise kann man in Javascript nicht nur Strings mit Strings, sondern auch Strings mit Zahlen konkatieren. Jedoch ist es wichtig, dass man hierbei auf die richtige Reihenfolge achtet. Schau dir dazu das folgende Beispiel an:

```Javascript
let num1 = 5;
let num2 = 3;
let result = "Die Summe von " + num1 + " und " + num2 + " ist " + (num1 + num2) + ".";
```

Das Ergebnis wäre hier: "Die Summe von 5 und 3 ist 8.". Wie du siehst, haben wir die Klammern um `(num1 + num2)` gesetzt, um sicherzustellen, dass die Zahlen zuerst addiert werden und dann erst konkatiniert werden.

Eine weitere wichtige Sache, die es zu beachten gibt, ist die Verwendung von Template-Strings durch die Verwendung von Backticks (`). Diese ermöglichen es dir, Variablen oder Ausdrücke direkt innerhalb eines Strings einzufügen. Schau dir dazu das folgende Beispiel an:

```Javascript
let name = "Marie";
let age = 25;
let message = `Ich heiße ${name} und ich bin ${age} Jahre alt.`;
```

Das Ergebnis wäre hier: "Ich heiße Marie und ich bin 25 Jahre alt.".

## Siehe auch

- [Javascript String API](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Using JavaScript Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [Javascript Basics: Concatenating Strings](https://www.digitalocean.com/community/tutorials/javascript-basics-concatenating-strings)