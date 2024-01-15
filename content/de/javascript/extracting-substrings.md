---
title:                "Unterstrings extrahieren"
html_title:           "Javascript: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substrings zu extrahieren, kann in vielen Fällen hilfreich sein, um Teile von Strings zu erhalten, die für unsere spezifischen Anforderungen relevant sind. Dies kann uns Zeit und Aufwand ersparen, da wir nicht den gesamten String durchsuchen müssen.

## Wie geht das?

Um Substrings in Javascript zu extrahieren, können wir die `substring()` oder `slice()` Methode verwenden. Diese beiden Methoden funktionieren ähnlich, aber es gibt ein paar Unterschiede, die wir im folgenden Beispiel sehen werden.

```Javascript
let string = "Willkommen in Javascript!";
string.substring(11, 20); // Output: Javascript
string.slice(11, 20); // Output: Javascript
string.substring(11, -1); // Output: Willkommen
string.slice(11, -1); // Output: Willkommen in Javascript
```

In diesem Beispiel haben wir einen String definiert und dann die `substring()` und `slice()` Methoden verwendet, um Substrings zu extrahieren. Der erste Parameter in beiden Methoden gibt die Startposition an, während der zweite Parameter die Endposition angibt (nicht inklusive). Der Unterschied liegt in der Verwendung von negativen Zahlen als zweiten Parameter. `substring()` interpretiert negative Zahlen als 0, während `slice()` sie als Endposition von hinten zählt.

Ein weiterer Unterschied zwischen den beiden Methoden ist, dass `slice()` auch negative Zahlen als ersten Parameter akzeptiert, um die Startposition von hinten zu zählen. Zum Beispiel `string.slice(-10) // Output: Javascript!`.

Wir können auch die `indexOf()` Methode kombinieren, um den Startpunkt für Substrings zu finden. Schauen wir uns dazu folgendes Beispiel an:

```Javascript
let string = "Willkommen in Javascript!";
let start = string.indexOf("Javascript"); // Output: 11
string.substring(start); // Output: Javascript!
string.slice(start); // Output: Javascript!
```

Hier haben wir zuerst die Startposition des Wortes "Javascript" mit `indexOf()` ermittelt und dann die `substring()` und `slice()` Methode verwendet, um den gewünschten Teil des Strings zu erhalten.

## Tiefer eintauchen

Es gibt auch andere Möglichkeiten, Substrings in Javascript zu erhalten, z.B. die `substr()` Methode. Diese Methode nimmt nur zwei Parameter an: die Startposition und die Anzahl der zu extrahierenden Zeichen.

```Javascript
let string = "Willkommen in Javascript!";
string.substr(11, 10); // Output: Javascript
```

Außerdem gibt es noch eine `split()` Methode, mit der wir einen String in ein Array aufteilen und dann den gewünschten Teil auswählen können.

```Javascript
let string = "Willkommen in Javascript!";
let array = string.split(" "); // Output: ["Willkommen", "in", "Javascript!"]
array[2]; // Output: Javascript!
```

Es ist wichtig zu beachten, dass die `substring()`, `slice()` und `substr()` Methoden teilweise in ihrem Verhalten voneinander abweichen. Es ist daher wichtig, sich mit den Unterschieden vertraut zu machen und die richtige Methode für den jeweiligen Anwendungsfall auszuwählen.

## Siehe auch

- [MDN Web Docs: substring()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs: slice()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Web Docs: substr()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN Web Docs: split()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/split)