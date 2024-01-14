---
title:                "Javascript: Eine Zeichenfolge großschreiben"
simple_title:         "Eine Zeichenfolge großschreiben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Capitalizing Strings ist eine grundlegende Funktion in der Javascript Programmierung, die oft verwendet wird, um die Darstellung von Text zu verändern. Es kann hilfreich sein, um die Lesbarkeit bestimmter Wörter oder Sätze zu erhöhen oder um bestimmte Formatierungsanforderungen zu erfüllen.

## Wie geht es

Um einen String in Javascript zu kapitalisieren, können Sie die `toUpperCase()` Methode verwenden. Diese Methode ändert alle Buchstaben in einem String in Großbuchstaben.

```Javascript
let string = "hallo welt";
let capitalizedString = string.toUpperCase();
console.log(capitalizedString);
```
Ausgabe:
```
HALLO WELT
```

Wenn Sie jedoch nur den Anfangsbuchstaben eines Strings groß schreiben möchten, können Sie die `charAt()` und `toUpperCase()` Methoden kombinieren. Die `charAt()` Methode gibt den Zeichenwert an der angegebenen Position im String zurück. Dies ermöglicht es uns, den ersten Buchstaben des Strings auszuwählen, ihn in einen Großbuchstaben zu konvertieren und ihn mit dem restlichen String zu verbinden.

```Javascript
let string = "javascript ist toll";
let capitalizedString = string.charAt(0).toUpperCase() + string.slice(1);
console.log(capitalizedString);
```

Ausgabe:
```
Javascript ist toll
```

## Tiefer eintauchen

Während die oben genannten Methoden eine einfache Möglichkeit bieten, Strings in Javascript zu kapitalisieren, gibt es auch andere Möglichkeiten, die genutzt werden können. Eine Möglichkeit ist die Verwendung von regulären Ausdrücken, um bestimmte Regeln für die Großschreibung festzulegen.

Ein anderer wichtiger Aspekt bei der Großschreibung von Strings ist, wie verschiedene Sprachen damit umgehen. Zum Beispiel können im Deutschen bestimmte Substantive immer groß geschrieben werden, während im Englischen nur der erste Buchstabe eines Satzes groß geschrieben wird. Für die sprachspezifische Großschreibung gibt es auch Bibliotheken und Tools, die dabei helfen können.

## Siehe auch

- [MDN Dokumentation für die toUpperCase() Methode](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Reguläre Ausdrücke in Javascript](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [String Großschreibung in verschiedenen Sprachen](https://www.techonthenet.com/js/string_toUpperCase.php)