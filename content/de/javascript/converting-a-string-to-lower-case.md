---
title:    "Javascript: Umwandlung eines Strings in Kleinbuchstaben"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

Beim Programmieren in Javascript gibt es oft Situationen, in denen es erforderlich ist, einen Text in Kleinbuchstaben zu konvertieren. Dies kann zum Beispiel bei der Validierung von Benutzereingaben oder beim Vergleichen von Zeichenketten verwendet werden. In diesem Blog-Post werde ich Ihnen zeigen, wie Sie eine Zeichenkette in Javascript in Kleinbuchstaben umwandeln können.

## Wie man es macht

Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, gibt es in Javascript die Methode `toLowerCase()`. Diese wird auf die zu konvertierende Zeichenkette angewendet und gibt die Zeichenkette in Kleinbuchstaben zurück. Im Folgenden finden Sie ein Beispiel:

```Javascript
let name = "Max Mustermann";
console.log(name.toLowerCase());
```

Dieser Code gibt folgende Ausgabe: `max mustermann`. Wie Sie sehen können, wird die Zeichenkette in Kleinbuchstaben umgewandelt. Wenn Sie die konvertierte Zeichenkette in einer Variablen speichern möchten, können Sie dies ebenfalls tun:

```Javascript
let name = "Max Mustermann";
let newName = name.toLowerCase();
console.log(newName);
```

Dieser Code gibt die gleiche Ausgabe: `max mustermann`.

## Tiefere Einblicke

Es gibt einige Dinge, die Sie bei der Verwendung der `toLowerCase()`-Methode beachten sollten. Zum Beispiel ist die Konvertierung von Zeichenketten in Kleinbuchstaben sprachspezifisch. Das bedeutet, dass das Ergebnis je nach verwendeter Sprache variieren kann. Es ist wichtig zu wissen, dass diese Methode nicht nur Buchstaben in Großbuchstaben umwandelt, sondern auch Sonderzeichen und Zahlen unverändert lässt. Außerdem sollten Sie beachten, dass die `toLowerCase()`-Methode keine Auswirkungen auf die ursprüngliche Zeichenkette hat, sondern eine neue Zeichenkette zurückgibt.

## Siehe auch

- [String.prototype.toLowerCase() Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Javascript Zeichenketten](https://www.javascript.com/learn/strings)