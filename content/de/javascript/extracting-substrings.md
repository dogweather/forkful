---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extrahieren von Unterteilen in JavaScript

## Was & Warum?

Das Extrahieren von Teilen von Strings, auch bekannt als Unterteil, ist ein üblicher Prozess in der Programmierung. Es erlaubt uns, spezifische Segmente von Textdaten zu isolieren und mit diesen zu arbeiten.

## So geht's:

In JavaScript gibt es mehrere Methoden, um String-Unterteile zu extrahieren: `substring()`, `slice()`, und `substr()`.

```Javascript  
let str = "Hallo, Welt!";

// Mit substring()
let teilStr = str.substring(0, 5);
console.log(teilStr); // "Hallo"

// Mit slice()
teilStr = str.slice(7);
console.log(teilStr); // "Welt!"

// Mit substr()
teilStr = str.substr(7, 5);
console.log(teilStr); // "Welt"
```
Jede dieser Methoden hat ihre eigenen Merkmale und Bedingungen. Sie wählen die geeignete Methode basierend auf Ihren Bedürfnissen.

## Vertiefung

Die Wahl der Methode zum Extrahieren von Teilen eines Strings ist historisch gewachsen. Die `substring()` und `slice()` Methoden sind aus den frühen JavaScript-Versionen übrig geblieben.

Einige Programmiersprachen bauen auf dieser Idee auf und fügen zusätzliche Funktionen hinzu, um das Arbeiten mit Text zu erleichtern (z.B. "split()", "join()", etc.). Aber die Grundlage bleibt: Das Extrahieren von String-Unterteilen ist ein grundlegender Teil des Arbeitens mit Text in fast jeder Programmiersprache.

## Weiterführende Links

Die vollständige Dokumentation zu diesen Methoden und einigen weiteren, die mit Strings in JavaScript arbeiten, finden Sie in der [Mozilla Developer Network Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String).

Eine zusätzliche Ressource, die beim Arbeiten mit Strings hilfreich sein könnte, ist [W3Schools](https://www.w3schools.com/jsref/jsref_obj_string.asp), die bietet interaktive Beispiele und eine breite Palette von Tutorials zu ähnlichen Themen.

Beachten Sie bitte, dass jede Methode ihre eigene spezifische Verhaltensweise hat, insbesondere bei der Behandlung von Randbedingungen und ungültigen Eingabewerten. Es ist ratsam, diese in der Dokumentation nachzuschlagen, um sie richtig zu verwenden.