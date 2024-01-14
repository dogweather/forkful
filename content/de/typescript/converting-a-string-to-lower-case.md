---
title:    "TypeScript: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Warum

Die Umwandlung von Zeichenketten in Kleinbuchstaben ist eine gängige Aufgabe in der Programmierung, insbesondere wenn es um die Verarbeitung von Benutzereingaben oder Daten aus externen Quellen geht. Indem man Zeichenketten in Kleinbuchstaben umwandelt, wird die Vergleichbarkeit und Verarbeitung von Daten erleichtert.

##Wie man Zeichenketten in Kleinbuchstaben umwandelt

```TypeScript
let string = "Hallo, WORLD!";
console.log(string.toLowerCase());
```
Output: "hallo, world!"

Die Methode "toLowerCase()" wird auf einer Zeichenkette angewendet und gibt eine neue Zeichenkette zurück, in der alle Buchstaben in Kleinbuchstaben umgewandelt wurden. Dies funktioniert für alle Sprachen, nicht nur für Englisch.

##Tiefergehende Informationen über die Umwandlung von Zeichenketten in Kleinbuchstaben

Bei der Umwandlung von Zeichenketten in Kleinbuchstaben muss beachtet werden, dass nicht alle Schriftzeichen und Sonderzeichen in allen Sprachen automatisch korrekt umgewandelt werden. Manche Sprachen haben Buchstaben, die aus mehreren Zeichen bestehen oder spezielle Akzente und Symbole, die in der Umwandlung möglicherweise verloren gehen.

Es ist auch wichtig zu wissen, dass die Umwandlung in Kleinbuchstaben keine Änderungen an der ursprünglichen Zeichenkette vornimmt, sondern eine neue Zeichenkette zurückgibt. Deshalb ist es wichtig, die Ergebnisse der Umwandlung in einer neuen Variablen zu speichern.

##Siehe auch

- [MDN Web Docs - String.toLowerCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [W3Schools - TypeScript Strings](https://www.w3schools.com/js/js_string_methods.asp)
- [Codecademy - Intro to TypeScript](https://www.codecademy.com/learn/introduction-to-typescript)