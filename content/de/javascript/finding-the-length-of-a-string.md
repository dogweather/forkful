---
title:                "Die Länge eines Strings finden"
html_title:           "Javascript: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Finden der Länge eines Strings ist ein häufiger Vorgang in der Programmierung, der es uns ermöglicht, die Anzahl der Zeichen in einem Text auszuwerten. Dies ist besonders nützlich, wenn wir z.B. überprüfen wollen, ob die Eingabe eines Benutzers die richtige Länge für ein Passwort oder eine Telefonnummer hat.

## Wie geht das?
Mit Javascript ist es sehr einfach, die Länge eines Strings zu finden. Dafür verwenden wir die eingebaute Funktion ```length```, die auf jedem String aufgerufen werden kann. Hier ist ein Beispiel:

```
// Deklaration einer Variable "text" mit einem Wert
let text = "Hallo Welt!";

// Verwendung der "length" Funktion, um die Länge des Strings zu finden
let laenge = text.length;

// Ausgabe der Länge in der Konsole
console.log(laenge); // Output: 11
```

## Tiefergehende Informationen
Die Funktion ```length``` gibt uns die Anzahl der Zeichen in einem String zurück, einschließlich Leerzeichen und Sonderzeichen. Sie ist sehr effizient und kann auf allen modernen Webbrowsern und Plattformen verwendet werden.

Alternativ können wir auch die Javascript-Methode ```String.prototype.size``` verwenden, die das gleiche Ergebnis liefert. Allerdings ist sie nicht so weit verbreitet und wird daher nicht so oft genutzt.

## Weitere Quellen
- [MDN Web Docs: String.prototype.length](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN Web Docs: String.prototype.size()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/size)