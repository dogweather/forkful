---
title:                "Javascript: HTML analysieren"
simple_title:         "HTML analysieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# Warum 

HTML ist die grundlegende Struktur, die das Aussehen und die Funktionalität von Websites bestimmt. Wenn du eine Website aufbauen möchtest, musst du das HTML verstehen und manipulieren können, um deine Anforderungen zu erfüllen. Das Parsen von HTML ist ein wichtiger Schritt, um dynamische und interaktive Websites zu erstellen, und ermöglicht es dir, spezifische Informationen aus dem HTML-Code auszulesen und zu verarbeiten.

# Wie geht das?

```Javascript
var html = "<div><h1>Hello, World!</h1> <p>This is a sample paragraph</p></div>";
var parser = new DOMParser();
var doc = parser.parseFromString(html, "text/html");
var heading = doc.querySelector("h1").innerHTML;
var paragraph = doc.querySelector("p").innerHTML;

console.log(heading);
console.log(paragraph); 
```

Ausgabe:

```
Hello, World!
This is a sample paragraph
```

Der obige Code zeigt, wie du den DOMParser in Javascript verwenden kannst, um HTML-Code zu analysieren und daraus Informationen zu extrahieren. Zuerst wird der HTML-Code in einer String-Variablen gespeichert. Dann wird der DOMParser aufgerufen, der den HTML-Code analysiert und in ein Document-Objekt umwandelt. Mit Hilfe von Methoden wie `querySelector()` können dann spezifische Elemente innerhalb des Dokuments ausgewählt werden, um auf deren Eigenschaften und Werte zuzugreifen. 

# Deep Dive 

Das Parsen von HTML kann manchmal komplexer werden, wenn es um komplexe HTML-Dokumente mit verschachtelten Elementen und Attributen geht. Eine Möglichkeit, dieser Komplexität zu begegnen, ist die Verwendung von Bibliotheken wie jQuery, die speziell für die Manipulation und Navigation von HTML-Code entwickelt wurden. Diese Bibliotheken bieten praktische Funktionen und Methoden, die das Parsen von HTML vereinfachen und beschleunigen. 

Ein weiterer wichtiger Aspekt beim Parsen von HTML ist die Validierung des Codes. Wenn deine Software oder Anwendung auf die Verarbeitung von HTML-Code angewiesen ist, ist es wichtig sicherzustellen, dass der Code gültig ist, um Fehler und unerwartete Ergebnisse zu vermeiden. Es gibt Online-Tools wie den HTML Validator von W3C, mit dem du HTML-Code auf seine Gültigkeit überprüfen kannst. 

# Siehe auch 

- [Einführung in das Parsen von HTML](https://www.w3schools.com/js/js_html_dom.asp)
- [DOMParser Referenz](https://developer.mozilla.org/de/docs/Web/API/DOMParser)
- [jQuery Dokumentation](https://jquery.com/)
- [W3C HTML Validator](https://validator.w3.org/)