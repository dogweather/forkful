---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen von Texten bezeichnet in der Informatik die Funktion, bestimmte Zeichenketten durch eine andere zu ersetzen. Programmierer verwenden es, um ihre Arbeit zu optimieren, indem sie redundante oder falsche Teile ihres Codes schnell ändern.

## Wie geht das?

```Javascript
// Erstellen Sie eine Beispielszeichenkette
let myString = "Guten Tag, Deutschland!";

// Suchen und ersetzen Sie Text im String
let newString = myString.replace("Deutschland", "Welt");

// Ausgabe: "Guten Tag, Welt!"
console.log(newString);
```

Mit dem `.replace()` Methoden können Sie einen Teil des Strings suchen und ersetzen.

## Vertiefung

Die Programmierung hat sich im Laufe ihrer Geschichte enorm weiterentwickelt, und das Suchen und Ersetzen von Texten ist nicht anders. Früher war dies ein umständlicher Prozess, der eine intensive manuelle Arbeit erforderte. Aber heutzutage können Programmierer mithilfe von integrierten Funktionen wie `.replace()` in JavaScript diese Aufgaben in Sekunden erledigen.

Alternativ haben Programmierer auch die Möglichkeit, Regular Expressions zu verwenden, was ihnen mehr Flexibilität bietet:
```Javascript
let regex = /Deutschland/gi;
let newString = myString.replace(regex, "Welt");
console.log(newString);
```

Das JavaScript `.replace()` verhält sich eigentlich ziemlich präzis und direkt. Es sucht nach dem ersten Parameter in der Quellzeichenkette und ersetzt ihn durch den zweiten Parameter. Es sollte jedoch beachtet werden, dass `.replace()` nur das erste Auftreten des Suchtexts im String ändert. Wenn mehrere Instanzen ersetzt werden sollen, muss ein regulärer Ausdruck mit einem globalen Kennzeichen (`/g`) verwendet werden.

## Weiterführende Links

Für mehr Informationen, schauen Sie bitte auf diese Quellen:

- [Mozilla Developer Network: String.prototype.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [JavaScript.Info: Basic Regular Expressions](https://javascript.info/regular-expressions)