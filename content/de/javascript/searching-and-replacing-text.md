---
title:                "Javascript: Textsuche und -ersetzung"
simple_title:         "Textsuche und -ersetzung"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Suchen und Ersetzen von Text im Programmieren ist ein nützliches Werkzeug, um effizient fehlerhafte oder veraltete Texte in einem Code zu ersetzen. Dies kann dazu beitragen, die Lesbarkeit und Funktionalität des Codes zu verbessern.

## Wie man es macht

Die Grundidee beim Suchen und Ersetzen von Text in Javascript ist, dass man nach einem bestimmten Text sucht und ihn durch einen anderen ersetzt. Dies kann entweder in einem bestimmten Bereich oder im gesamten Code durchgeführt werden. Zum Beispiel kann man alle Vorkommnisse von "Hund" durch "Katze" ersetzen oder nur innerhalb einer bestimmten Funktion.

```Javascript
// Einfaches Beispiel für die Verwendung von String.replace()
let string = "Ich habe einen Hund namens Max.";
let newString = string.replace("Hund", "Katze");

console.log(newString);
// Ausgabe: "Ich habe eine Katze namens Max."
```

Man kann auch reguläre Ausdrücke verwenden, um nach bestimmten Mustern oder Zeichenfolgen zu suchen und zu ersetzen. Hier ist ein Beispiel, bei dem alle Zahlen in einer Zeichenfolge durch Sternchen ersetzt werden:

```Javascript
let string = "Ich gehe am 01. Dezember zum Sport.";
let newString = string.replace(/[0-9]/g, "*");

console.log(newString);
// Ausgabe: "Ich gehe am **. Dezember zum Sport." 
```

Es ist auch möglich, den ersetzten Text dynamisch aus einer Funktion zu generieren. Hier ist ein Beispiel, bei dem das aktuelle Datum als Ersatz für einen Platzhalter verwendet wird:

```Javascript
let string = "Heute ist [current_date].";
let newString = string.replace("[current_date]", new Date().toDateString());

console.log(newString);
// Ausgabe: "Heute ist Sat Sep 26 2020."
```

## Tiefergehende Informationen

Für komplexere Aufgaben des Suchens und Ersetzens von Text gibt es in Javascript auch die Methode String.replaceAll(), die seit dem ES2021 Update verfügbar ist. Diese Methode ermöglicht es, alle Vorkommnisse eines Musters auf einmal zu ersetzen, anstatt nur das erste oder alle nach der ersten Suche.

Es ist auch wichtig zu beachten, dass bei der Verwendung von regulären Ausdrücken beim Ersetzen die Groß- und Kleinschreibung berücksichtigt wird. Wenn man dies vermeiden möchte, kann man die Flags "g" und "i" verwenden, um die Suche global und case-insensitive zu gestalten.

## Siehe auch

Hier sind einige Links, die dir helfen können, mehr über das Suchen und Ersetzen von Text in Javascript zu erfahren:

- [MDN Web Docs: String.replace()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: String.replaceAll()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll)
- [W3Schools: JavaScript Regular Expressions](https://www.w3schools.com/js/js_regexp.asp)