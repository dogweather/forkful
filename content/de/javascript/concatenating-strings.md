---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Verkettung von Strings (Zeichenketten) in Javascript bedeutet einfach die Kombination von zwei oder mehr Strings. Entwickler verwenden sie, um dynamische Werte in Text zu integrieren und komplexe Meldungen aus einfacheren Teilen zu erstellen. 

## Anleitung:

Es gibt mehrere Möglichkeiten, Strings in Javascript zu verkettten. Hier sind einige Beispiele:

Die alte Methode mittels `+` Operator:
```Javascript
let string1 = "Hallo, ";
let string2 = "Welt!";
let result = string1 + string2;
console.log(result); // Ausgabe: "Hallo, Welt!"
```
Die moderne Methode mittels Template Literals (`` `${}` `` Formatierung):

```Javascript
let string1 = "Hallo, ";
let string2 = "Welt!";
let result = `${string1}${string2}`;
console.log(result); // Ausgabe: "Hallo, Welt!"
```
## Vertieftes Wissen:

Die `+` Operator Methode zur Verkettung von Strings existiert seit den Anfangstagen von JavaScript, ist jedoch hinsichtlich Lesbarkeit und Fehlersuche nicht optimal. Dies hat zur Einführung der Template-Literals-Methode in ES6 geführt, die sich besser für längere Strings mit mehreren Variablen und speziellen Zeichen eignet.

Es gibt auch alternative Methoden, wie z. B. die `concat()`-Funktion oder das `join()`-Verfahren auf Arrays. Allerdings bevorzugen viele Entwickler die Nutzung der Template-Literals-Methode aufgrund ihrer intuitiven Syntax und Flexibilität.

Die Wahl der Methode hängt von mehreren Faktoren ab, einschließlich der Codestil-Vorgaben des Projekts und der Komplexität der zu verknüpfenden Strings.

## Weiterführende Informationen

1. [MDN Web Docs: Template Strings](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/template_strings)
2. [Javascript.info: Strings](https://javascript.info/string)
3. [W3Schools: JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)