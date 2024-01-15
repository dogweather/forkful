---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Javascript: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Manchmal ist es notwendig, bestimmte Zeichen in einem Text zu löschen, die einem bestimmten Muster entsprechen. Dies kann hilfreich sein, um unerwünschte Daten zu entfernen oder um bestimmte Formatierungen anzupassen. Mit Javascript ist es sehr einfach, solche Zeichen zu löschen, und in diesem Artikel werden wir sehen, wie das geht.

## Wie es geht

Um Zeichen in Javascript zu löschen, müssen wir den String-Methode `replace()` verwenden. Diese Methode akzeptiert ein reguläres Ausdrucksmuster als ersten Parameter und den zu ersetzenden Text als zweiten Parameter. Hier ist ein Beispiel, um alle Leerzeichen in einem String zu entfernen:

```Javascript
let string = "Dies ist ein Beispiel Text.";
let neueString = string.replace(/\s/g, "");
console.log(neueString);
// Ausgabe: "DiesisteinBeispielText."
```

In diesem Beispiel haben wir den regulären Ausdruck `/s/g` verwendet, der alle Leerzeichen (`\s`) global (`g`) im Text ersetzt haben. Mit diesem Muster können wir auch andere Zeichen löschen, indem wir einfach den entsprechenden Ausdruck anpassen. Zum Beispiel können wir alle Zahlen aus einem String entfernen, indem wir `/[0-9]/g` als regulären Ausdruck verwenden.

## Tiefere Einblicke

Die Methode `replace()` ermöglicht es uns auch, eine Funktion als zweiten Parameter zu übergeben. Dadurch können wir das zu ersetzende Zeichen dynamisch basierend auf bestimmten Bedingungen auswählen oder den bereits vorhandenen Text manipulieren. Hier ist ein Beispiel, um nur jeden zweiten Buchstaben in einem String zu löschen:

```Javascript
let string = "Dies ist ein Beispiel Text.";
let neueString = string.replace(/./g, (match, index) => index % 2 === 0 ? match : "");
console.log(neueString);
// Ausgabe: "Dei ti snBipel ex."
```

In diesem Beispiel haben wir die Funktion `match` verwendet, um jeden zweiten Buchstaben basierend auf seinem Index im String zu löschen. Mit dieser Methode können wir auch komplexere Logik implementieren, um bestimmte Zeichen zu löschen oder zu ersetzen.

## Siehe auch

- [Reguläre Ausdrücke in Javascript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [String-Methode replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)