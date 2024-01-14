---
title:                "Javascript: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in vielen Fällen notwendig sein, um eine saubere und strukturierte Codebasis zu gewährleisten. Dies kann besonders hilfreich sein, wenn man mit großen Datenmengen arbeitet oder bestimmte Strings oder Zeichenketten in einem Code blockieren möchte.

# Wie geht's

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können wir die `replace()` Funktion von Javascript nutzen. Diese Funktion ermöglicht es uns, eine Zeichenkette zu durchsuchen und entsprechend zu manipulieren. Hier ist ein Beispielcode, der alle Buchstaben "a" aus einer Zeichenkette löscht:

```Javascript
let string = "Dieser Satz enthält viele 'a' Buchstaben.";
string = string.replace(/a/g, ""); // Hier wird 'a' durch einen leeren String ersetzt
console.log(string); // Ausgabe: "Dieser Stz enthält viele ' Buchstaben."
```

In diesem Beispiel haben wir das Pattern `/a/g` in der `replace()` Funktion verwendet. Das Pattern "a" bedeutet, dass jedes Vorkommen des Buchstaben "a" gelöscht wird, während das Flag "g" dafür sorgt, dass dies global in der gesamten Zeichenkette geschieht. Dies ist besonders nützlich, wenn wir nicht mehrere Befehle ausführen möchten, um jedes Vorkommen zu löschen.

# Eine tiefere Analyse

Die `replace()` Funktion ist eine mächtige und vielseitige Methode, die uns mehrere Möglichkeiten bietet, Zeichen zu löschen, die einem bestimmten Muster entsprechen. Wir können auch eine Funktion als zweiten Parameter in der `replace()` Funktion angeben, um bestimmte Bedingungen zu erfüllen, bevor wir ein Vorkommen ersetzen oder löschen. Zum Beispiel können wir alle Zahlen aus einer Zeichenkette entfernen, die größer als 5 sind.

Hier ist ein Beispiel, das diese Funktionalität demonstriert:

```Javascript
let string = "1, 3, 7, 8, 12, 9";
string = string.replace(/\d+/g, function(match) {
    if (match > 5) {
        return ""; // Löschen der Zahl, wenn sie größer als 5 ist
    } else {
        return match; // Andernfalls wird die Zahl unverändert gelassen
    }
});
console.log(string); // Ausgabe: "1, 3, , , , "
```

In diesem Beispiel haben wir das Pattern `/d+/g` verwendet, um alle Zahlen in der Zeichenkette zu finden, und eine Funktion als zweiten Parameter angegeben, die die Bedingung für das Löschen überprüft.

# Siehe auch

- [Javascript: String replace() Methode](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace) (MDN)
- [Javascript RegExp Objekt](https://www.w3schools.com/jsref/jsref_obj_regexp.asp) (W3Schools)
- [RegExr: Eine Live Regex Test Umgebung](https://regexr.com/) (Website)