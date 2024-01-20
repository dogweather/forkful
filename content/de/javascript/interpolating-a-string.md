---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die String-Interpolation in Javascript ist eine Technik, bei der Werte dynamisch in einen String eingefügt werden. Es ermöglicht einen saubereren, lesbareren Code und erleichtert die Manipulation von Zeichenketten.

## So geht's:

Mit den Template Literalen, bietet Javascript eine einfache Art der String-Interpolation. Hier ein Beispiel:

```Javascript
let name = 'Max';
let begruessung = `Hallo ${name}!`;
console.log(begruessung);  // Ausgabe: "Hallo Max!"
```
In diesem Code wird der Wert der Variablen `name` dynamisch in den String `begruessung` eingefügt.


## Vertiefung:

Die String-Interpolation wurde erst mit ES2015/ES6 eingeführt. Vorher musste man noch umständlich "+" verwenden um Strings und Variablen zu verknüpfen, was leicht zu Fehlern führen kann.

Alternativen zur String-Interpolation sind die bereits genannte Methode mit "+", oder Funktionen wie `.concat()`, oder `.join()`, die allerdings weniger intuitiv und schwerer zu lesen sind.

Bei der Umsetzung der String-Interpolation ist zu beachten, dass nur innerhalb der Backticks (` `) Variablen und Ausdrücke verwendet werden können. Diese werden dann in den String interpoliert. Weiterhin sind auch Ausdrücke wie `Hallo ${1+1}!` möglich, was "Hallo 2!" ausgeben würde.

## Siehe auch:

[MDN Web Docs - Template Literale](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Template_literals)

[Javascript.info - Template Literale](https://javascript.info/string#template-literals)

[Freecodecamp - A Deep Dive Into ES6 Template Literals](https://www.freecodecamp.org/news/template-literals-in-es6-new-features-and-polymorphic-strings-84129283d377/)