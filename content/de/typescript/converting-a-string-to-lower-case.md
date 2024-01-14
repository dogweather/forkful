---
title:                "TypeScript: Ein String in Kleinbuchstaben umwandeln"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren einer Zeichenfolge in Kleinbuchstaben ist eine häufige Aufgabe beim Programmieren. Es kann nützlich sein, um eine einheitliche Formatierung in verschiedenen Teilen eines Programms zu gewährleisten oder um Vergleiche zwischen Zeichenfolgen unabhängig von Groß- und Kleinschreibung durchzuführen.

## Wie geht man vor

```TypeScript
const string = "Hallo Welt";
const lowerCaseString = string.toLowerCase();
console.log(lowerCaseString);
// Output: hallo welt
```

Der Vorgang ist sehr einfach und kann mit der Methode `toLowerCase()` auf einer Zeichenfolge durchgeführt werden. Diese Methode gibt eine neue Zeichenfolge in Kleinbuchstaben zurück und verändert nicht die ursprüngliche Zeichenfolge. Dies ermöglicht es, die unveränderte Zeichenfolge später wiederzuverwenden, wenn nötig.

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken, um alle Großbuchstaben in einer Zeichenfolge zu finden und durch ihre entsprechenden Kleinbuchstaben zu ersetzen.

```TypeScript
const string = "Hello World";
const lowerCaseString = string.replace(/[A-Z]/g, (match) => match.toLowerCase());
console.log(lowerCaseString);
// Output: hello world
```

## Tiefgehende Informationen

Während die Konvertierung einer Zeichenfolge in Kleinbuchstaben eine einfache Aufgabe ist, gibt es einige wichtige Dinge zu beachten. Zum Beispiel kann der Vorgang bei Sprachen mit speziellen Großbuchstaben wie dem deutschen Umlaut (ä, ö, ü) oder dem Eszett (ß) zu unerwarteten Ergebnissen führen.

In solchen Fällen sollte man die Methode `toLocaleLowerCase()` anstelle von `toLowerCase()` verwenden, um sicherzustellen, dass diese Sonderzeichen korrekt behandelt werden.

Es ist auch wichtig, die kulturelle oder regionale Sensibilität bei der Verwendung von `toLowerCase()` zu berücksichtigen. Die Ausgabe kann je nach Sprache, in der das Programm ausgeführt wird, variieren. In manchen Fällen kann es daher sinnvoller sein, eine andere Methode zur Konvertierung in Kleinbuchstaben zu verwenden, die speziell auf die betreffende Sprache zugeschnitten ist.

## Siehe auch

- [MDN Web Docs - String.prototype.toLowerCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)