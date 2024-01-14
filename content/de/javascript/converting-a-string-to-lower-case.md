---
title:                "Javascript: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Strings in Kleinbuchstaben kann hilfreich sein, wenn man eine Suchfunktion implementiert oder Texte vergleicht, da es die Unterschiede in der Groß- und Kleinschreibung eliminiert.

## Wie funktioniert es?

Das Konvertieren eines Strings in Kleinbuchstaben in Javascript ist sehr einfach. Es kann auf verschiedene Arten erreicht werden, aber wir verwenden die `toLowerCase()` Funktion. Hier ist ein Beispielcode:

```Javascript
let string = "Hallo WELT!";
let lowerCaseString = string.toLowerCase();
console.log(lowerCaseString);
```

Die Ausgabe dieses Codes wird `hallo welt!` sein. Wie Sie sehen können, wurde der ursprüngliche String in Kleinbuchstaben umgewandelt.

## Tiefer in die Materie

Wussten Sie, dass das Konvertieren eines Strings in Kleinbuchstaben auch Unicode-Buchstaben berücksichtigt? Das bedeutet, dass nicht nur a-z Buchstaben, sondern auch Buchstaben aus anderen Sprachen richtig in Kleinbuchstaben konvertiert werden.

Ein weiterer wichtiger Punkt ist, dass die `toLowerCase()` Funktion eine neue, konvertierte Version des Strings zurückgibt, anstatt den ursprünglichen String zu ändern. Wenn Sie also den ursprünglichen String in Kleinbuchstaben haben möchten, müssen Sie ihn der Funktion zuweisen.

## Siehe auch

- [MDN web docs: `toLowerCase()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [W3School: JavaScript strings](https://www.w3schools.com/js/js_strings.asp)
- [freeCodeCamp: Convert Strings to Lowercase](https://www.freecodecamp.org/news/javascript-string-methods-reference/)

Vielen Dank fürs Lesen! Wir hoffen, dieser Artikel hat Ihnen geholfen, das Konzept des Konvertierens eines Strings in Kleinbuchstaben in Javascript besser zu verstehen. Vergessen Sie nicht, verschiedene Möglichkeiten auszuprobieren und zu experimentieren. Viel Spaß beim Programmieren!