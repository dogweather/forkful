---
title:                "Das Parsen von HTML."
html_title:           "TypeScript: Das Parsen von HTML."
simple_title:         "Das Parsen von HTML."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Warum
Parsing HTML ist ein wichtiger Aspekt in der Webentwicklung, da es ermöglicht, die Struktur und Inhalte einer Webseite zu analysieren und zu manipulieren. Mit TypeScript können wir diesen Prozess noch effizienter und zuverlässiger gestalten.

## Wie geht's
Zur Durchführung eines HTML-Parsing in TypeScript verwenden wir die `htmlparser2`-Bibliothek. Beginnen wir mit der Installation:
```TypeScript
npm install htmlparser2
```
Als nächstes importieren wir die Bibliothek in unserem Code:
```TypeScript
import { Parser } from "htmlparser2";
```
Wir können nun einen einfachen HTML-Code in eine Zeichenfolge speichern und den Parser initialisieren:
```TypeScript
const html = "<div>Hello World</div>";
const parser = new Parser();
```
Um den Code zu parsen, können wir eine Funktion als zweites Argument der `write()`-Methode des Parsers übergeben, die jedes Mal aufgerufen wird, wenn ein neues Element gefunden wird:
```TypeScript
parser.write(html, (error: any, dom: any) => {
  // Hier können wir mit dem DOM-Baum arbeiten
  if (!error) {
    console.log(dom); // Gibt ein Array mit dem geparsten HTML-Baum zurück
  }
});
```
Das Ergebnis der Konsolenausgabe würde in diesem Fall so aussehen:
```TypeScript
[ { type: 'tag', name: 'div', attribs: {}, children: [ { data: 'Hello World', type: 'text', next: null, prev: null }, next: null, prev: null } ]
```
Wir können nun jede gewünschte Manipulation an diesem DOM-Baum durchführen und unser modifiziertes HTML dann mit der `end()`-Methode des Parsers wieder in eine Zeichenfolge umwandeln:
```TypeScript
const modifiedHTML = parser.end();
console.log(modifiedHTML);  // Ausgabe: <div>Bye World</div>
```

## Deep Dive
HTML-Parsing kann komplex werden, wenn der Code viele verschachtelte Elemente und Attribute enthält. In solchen Fällen kann es hilfreich sein, eine eingehende Analyse der Struktur des DOM-Baums durchzuführen und spezifische Elemente oder Attribute gezielt auszuwählen. Hierbei können die in der `Parser`-Klasse verfügbaren Methoden, wie z.B. `onopentag`, `onclosetag` und `onattribute`, nützlich sein. Diese können verwendet werden, um den DOM-Baum während des Parsing-Prozesses zu durchlaufen und bestimmte Operationen auf einzelne Elemente oder Attribute anzuwenden.

## Siehe auch
- [htmlparser2 Dokumentation] (https://www.npmjs.com/package/htmlparser2)
- [Artikel zu HTML-Parsing mit TypeScript] (https://blog.logrocket.com/parsing-html-using-typescript/)