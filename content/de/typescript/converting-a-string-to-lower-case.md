---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Umwandeln eines Strings in Kleinbuchstaben ist eine häufig benötigte Programmierfähigkeit. Damit können wir sicherstellen, dass alle Zeichen im String klein geschrieben sind. Das hilft uns dabei, menschliche Fehler zu vermeiden und bietet eine bessere Kontrolle über die Formatierung von Text in unserem Programm.

## So geht's:

Hier ist ein einfaches Beispiel, wie Sie einen String in TypeScript in Kleinbuchstaben umwandeln können:

```TypeScript
let text: string = "Hallo Welt!";
text = text.toLowerCase();
console.log(text);  // outputs: "hallo welt!"
```
In diesem Beispiel wird die Funktion "toLowerCase()" auf den String angewendet, um alle Zeichen in Kleinbuchstaben zu ändern. Das Ergebnis ist "hallo welt!".

## Deep Dive:

Historisch gesehen bieten fast alle modernen Programmiersprachen String Manipulationsfunktionen. In TypeScript ist "toLowerCase()" eine solche Funktion aus dem Standard-String-Objekt von JavaScript. 

Als Alternativen könnten Sie auch eine Schleife verwenden, um durch jeden Charakter im String zu gehen und ihn eigenständig zu ändern. Das ist jedoch bei weitem nicht so effizient oder lesbar wie die Nutzung von "toLowerCase()".

Die Implementation von "toLowerCase()" in JavaScript wird durch den ECMAScript Standard bestimmt. Die genauen technischen Details darüber, wie diese Funktion implementiert wird, sind abhängig von der JavaScript-Engine, die verwendet wird. 

## See Also:

- [TypeScript Einführung](https://www.typescriptlang.org/docs/) - Ein Guide zur Programmierung mit TypeScript.
- [String Manipulation in JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String) - Weiterführende Informationen zu Funktionen, die auf Strings angewendet werden können, einschließlich "toLowerCase()".