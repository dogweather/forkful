---
title:    "Javascript: Verwendung von regulären Ausdrücken"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Wenn du JavaScript programmierst, bist du wahrscheinlich schon auf Situationen gestoßen, in denen du Texte auf bestimmte Muster oder Formate überprüfen musstest. Hier kommen reguläre Ausdrücke ins Spiel! Als mächtiges Werkzeug ermöglichen sie es dir, solche Textmuster mithilfe weniger Zeichen und Befehle zu definieren und zu durchsuchen. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man reguläre Ausdrücke in JavaScript verwenden kann.

## Wie es geht

Reguläre Ausdrücke werden in JavaScript durch den `RegExp`-Konstruktor oder durch Literale, die mit `/` begrenzt sind, dargestellt. Es gibt mehrere nützliche Methoden und Eigenschaften, die mit regulären Ausdrücken verwendet werden können, um Texte zu durchsuchen und zu manipulieren.

```javascript
// Erstelle einen regulären Ausdruck, der nach allen Dateien mit der Endung `.js` sucht
const regex = new RegExp(/\.js$/);

// Alternativ können reguläre Ausdrücke auch als Literale geschrieben werden
const regex = /\.js$/;

// Verwende die `test`-Methode, um zu überprüfen, ob ein String mit dem regulären Ausdruck übereinstimmt
const string = "mein-script.js";
regex.test(string); // gibt true zurück

// Verwende die `exec`-Methode, um eine Übereinstimmung in einem String zu finden
const string = "Heute ist der 01.01.2021";
const dateRegex = /\d{2}\.\d{2}\.\d{4}/;
dateRegex.exec(string); //gibt ["01.01.2021"] zurück
```

## Tiefer Einblick

Reguläre Ausdrücke bieten viele fortgeschrittene Funktionen, die es dir ermöglichen, sehr komplexe Muster zu definieren. Hier sind einige weitere wichtige Konzepte:

- Quantifiers: Durch die Verwendung von Quantifiers wie `*` (null oder mehr), `+` (ein oder mehr) oder `?` (null oder eins) kannst du angeben, wie oft ein Teil des Musters wiederholt werden soll.
- Character classes: Mit Character classes wie `[a-z]` (alle kleinen Buchstaben) oder `[0-9]` (alle Zahlen) kannst du bestimmte Zeichengruppen definieren, die in einem Textmuster vorkommen können.
- Capturing groups: Mit Klammern `()` kannst du angeben, welche Teile eines Musters du extrahieren möchtest.
- Flags: Flags wie `i` (ignoriere Groß- und Kleinschreibung) oder `g` (global) können verwendet werden, um das Verhalten eines regulären Ausdrucks zu ändern.

## Siehe auch

- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools: JavaScript RegExp](https://www.w3schools.com/js/js_regexp.asp)
- [regex101: Online RegEx Tester und Debugger](https://regex101.com/)