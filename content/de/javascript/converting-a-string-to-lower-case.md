---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Javascript: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Warum würde man sich dafür entscheiden, einen String in Kleinbuchstaben umzuwandeln? Das kann verschiedene Gründe haben. Vielleicht muss der String für einen spezifischen Algorithmus in einer bestimmten Formatierung vorliegen, oder man möchte einen String für einen Vergleich mit einem anderen String angleichen. In jedem Fall kann das Umwandeln eines Strings in Kleinbuchstaben nützlich sein, um genauer arbeiten zu können.

## Wie man es macht

Um einen String in Kleinbuchstaben umzuwandeln, gibt es in Javascript die nützliche Methode `toLowerCase()`. Diese Methode wird direkt auf einen String angewendet und gibt eine neue Version des Strings zurück, in der alle Buchstaben in Kleinbuchstaben umgewandelt wurden.

```Javascript
let string = "Hallo WELT";
let lowerCase = string.toLowerCase();

console.log(lowerCase); // "hallo welt"
```

Wie man in dem Beispiel sehen kann, wurde der ursprüngliche String "Hallo WELT" mithilfe der `toLowerCase()` Methode in den String "hallo welt" umgewandelt.

## Tieferer Einblick

Bei der Umwandlung eines Strings in Kleinbuchstaben gibt es einige Dinge zu beachten. Zum einen ist es wichtig zu wissen, dass die `toLowerCase()` Methode nicht nur Buchstaben in der deutschen Sprache, sondern auch andere Sonderzeichen in Kleinbuchstaben umwandelt. Dies kann manchmal zu unerwarteten Ergebnissen führen, vor allem wenn man mit Umlauten arbeitet.

Ein weiterer Aspekt, der bei der Umwandlung von Strings berücksichtigt werden sollte, ist die Unicode-Kodierung. Die `toLowerCase()` Methode arbeitet auf der Basis des Unicode-Standard, was bedeutet, dass sie auch internationale Zeichen korrekt umwandelt.

Beispiel:

```Javascript
let string = "FÜR IMMER";
let lowerCase = string.toLowerCase();

console.log(lowerCase); // "für immer"
```

In diesem Beispiel haben wir einen String mit einem deutschen Umlaut. Würde man nun versuchen, diesen mit gängigen Methoden wie `replace()` oder `toLowerCase()` in Kleinbuchstaben umzuwandeln, könnte es zu inkorrekten Ergebnissen führen. Hier ist die `toLowerCase()` Methode die bessere Wahl, da sie die Kodierung des Umlauts berücksichtigt und das richtige Ergebnis liefert.

## Siehe auch

Hier sind einige weitere Ressourcen, die sich mit dem Thema der String-Umwandlung und dem Unicode-Standard beschäftigen:

- MDN Web Docs: [toLocaleLowerCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- ECMAScript Language Specification: [Unicode Case Mapping](https://tc39.es/ecma262/#sec-unicode-case-mapping)
- Stack Overflow: [Why does toLowerCase() return the wrong result with german umlauts?](https://stackoverflow.com/questions/49208259/why-does-tolowercase-return-the-wrong-result-with-german-umlauts)