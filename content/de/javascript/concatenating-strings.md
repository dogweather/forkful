---
title:    "Javascript: Strings verknüpfen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren in Javascript ist es manchmal erforderlich, Strings zusammenzufügen, um eine Gesamtausgabe zu erstellen. Dies kann dazu dienen, verschiedene Teile von Informationen zu einer lesbaren Nachricht oder zur Generierung dynamischer Inhalte auf einer Webseite zu kombinieren.

## Wie man Strings in Javascript zusammenfügt

Die einfachste Methode, um Strings in Javascript zu verbinden, ist die Verwendung des "+ (Plus)" Operators. Hier ein Beispiel:

```Javascript
var name = "Max";
var greeting = "Hallo " + name + "!";
console.log(greeting);
```

Die Ausgabe dieses Codes wird folgende Nachricht sein: "Hallo Max!". Die Variablen "name" und "greeting" werden mit dem "+" Operator zusammengefügt und als Ergebnis erscheint die auf der Konsole ausgegebene Nachricht.

Eine andere Möglichkeit, Strings zu verbinden, ist die Verwendung von Template Strings, auch bekannt als Template Literals. Beispiel:

```Javascript
var name = "Maria";
var greeting = `Hallo ${name}!`;
console.log(greeting);
```

Die Ausgabe dieses Codes wird die gleiche wie oben sein. Der Unterschied besteht darin, dass Template Strings die Verwendung von Variablen innerhalb von ${...} ermöglichen, was besonders nützlich ist, wenn innerhalb des Strings Berechnungen oder Funktionen verwendet werden sollen.

## Tiefergehender Einblick

Beim Zusammenfügen von Strings in Javascript ist es wichtig zu beachten, dass der Datentyp der Ausgabe vom Datentyp der Variablen abhängt, die zusammengefügt werden. Beispielsweise, wenn eine Variable vom Typ String und eine Variable vom Typ Integer (Zahl) miteinander verbunden werden, wird das Ergebnis eine String-Konkatenation sein.

Ein weiterer wichtiger Punkt ist die Reihenfolge der Verknüpfung. Bei der Verwendung von Template Strings wird die Reihenfolge der Verknüpfung gemäß der Position der Variablen im String befolgt. Beispiel:

```Javascript
var num1 = 5;
var num2 = 10;
var num3 = 15;
var calculation = `Das Ergebnis lautet: ${num3 + num2 / num1}`;
console.log(calculation);
```

Die Ausgabe dieses Codes wird "Das Ergebnis lautet: 17" sein, da zuerst die Division ausgeführt wird (10/5=2) und dann die Addition (2+15=17).

## Siehe auch

- [W3Schools Tutorial: Concatenation](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [MDN Web Docs: Template Literals](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/template_strings)
- [Stack Overflow: Best way to concatenate strings in JavaScript](https://stackoverflow.com/questions/5612787/best-way-to-concatenate-strings-in-javascript)