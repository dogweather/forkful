---
title:                "Javascript: Ein String großschreiben"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es oft die Notwendigkeit, Strings (Zeichenketten) in bestimmten Fällen zu modifizieren. Das kann unter anderem dazu dienen, den String in einem bestimmten Format darzustellen oder eine einheitliche Schreibweise zu gewährleisten. Eine häufige Anforderung ist das Großschreiben von Strings. Doch warum sollte man überhaupt einen String großschreiben?

## Wie geht das

Das Großschreiben eines Strings kann auf verschiedene Arten in Javascript umgesetzt werden. Eine Möglichkeit ist die Verwendung der `toUpperCase()` Funktion. Diese konvertiert alle Zeichen eines Strings in Großbuchstaben und gibt den modifizierten String zurück. Hier ein Beispiel:

```Javascript
const string = "hallo welt";
const capitalizedString = string.toUpperCase();
console.log(capitalizedString); // Ausgabe: HALLO WELT
```

Eine andere Möglichkeit ist die Verwendung der `replace()` Funktion. Mit Hilfe von regulären Ausdrücken kann man alle Kleinbuchstaben durch den entsprechenden Großbuchstaben ersetzen. Hier ein Beispiel:

```Javascript
const string = "hallo welt";
const capitalizedString = string.replace(/[a-z]/g, char => char.toUpperCase());
console.log(capitalizedString); // Ausgabe: HALLO WELT
```

## Tiefergehende Informationen

Das Großschreiben von Strings scheint auf den ersten Blick eine einfache Aufgabe zu sein. Doch es gibt einige Dinge zu beachten, wenn man sicherstellen möchte, dass ein String korrekt großgeschrieben wird. Zum Beispiel müssen Sonderzeichen und Umlaute beim Großschreiben berücksichtigt werden. Auch die Lokalisierung eines Programms kann Auswirkungen auf den Umgang mit Groß- und Kleinschreibung haben.

## Siehe auch

- [MDN Web Docs - String.prototype.toUpperCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Eintrag zu Groß- und Kleinschreibung auf Wikipedia](https://de.wikipedia.org/wiki/Gro%C3%9F-_und_Kleinschreibung)