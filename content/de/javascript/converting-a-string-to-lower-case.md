---
title:    "Javascript: In eine Kleinbuchstabenfolge umwandeln"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Stellen Sie sich vor, Sie haben einen Text in Großbuchstaben geschrieben und möchten diesen aus irgendeinem Grund in Kleinbuchstaben umwandeln. Oder vielleicht vergleichen Sie die Eingabe eines Benutzers mit einer gespeicherten Datenbank und möchten sicherstellen, dass die Groß- und Kleinschreibung übereinstimmt. In solchen Fällen ist es wichtig zu wissen, wie man eine Zeichenfolge in Javascript in Kleinbuchstaben konvertiert.

## Wie man eine Zeichenfolge in Kleinbuchstaben konvertiert

Zum Glück gibt es in Javascript eine integrierte Methode, um eine Zeichenfolge in Kleinbuchstaben umzuwandeln. Die Methode heißt "toLowerCase()" und kann auf einen String angewendet werden. Schauen wir uns ein Beispiel an:

```Javascript
let text = "HALLO WELT";
let convertedText = text.toLowerCase();
console.log(convertedText) // Output: hallo welt
```

In diesem Beispiel erstellen wir eine Variable namens "text", die den Text in Großbuchstaben enthält. Dann wenden wir die "toLowerCase()" Methode auf diese Variable an und speichern das Ergebnis in einer neuen Variable namens "convertedText". Wenn wir den Inhalt dieser Variablen ausgeben, sehen wir, dass der ursprüngliche Text nun in Kleinbuchstaben erscheint.

## Tieferes Eintauchen

Wenn Sie sich für eine tiefere Analyse dieser Konvertierungsmethode interessieren, gibt es ein paar Dinge zu beachten. Zum einen ist diese Methode nicht nur auf deutsche Texte beschränkt, sie kann auf allen Sprachen angewendet werden. Auch Sonderzeichen werden korrekt umgewandelt. Zum Beispiel wird aus dem deutschen "Österreich" korrekt "österreich".

Ein weiteres interessantes Detail ist, dass diese Methode im Gegensatz zur "toUpperCase()" Methode, die eine Zeichenfolge in Großbuchstaben umwandelt, keine Parameter akzeptiert. Das bedeutet, dass es keine Möglichkeiten gibt, die Konvertierung zu beeinflussen. Dies kann für manche Anwendungen möglicherweise problematisch sein, da sie keine Kontrolle darüber haben, wie die Zeichenfolge konvertiert wird.

## Siehe auch

- [MDN Dokumentation - String toLowerCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [W3Schools - Javascript String toLowerCase() Beispiel](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [GeeksforGeeks - Javascript String toLowerCase() Beispiel](https://www.geeksforgeeks.org/javascript-tostring-tolowercase-function/)