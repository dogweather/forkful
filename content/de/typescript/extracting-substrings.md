---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Extraktion von Teilstrings ist ein Prozess, bei dem spezifische Teile einer Zeichenkette ausgewählt und isoliert werden. Programmierer machen dies, um bestimmte Daten aus einem größeren Text zu isolieren und zu bearbeiten.

## Anleitung:

Um Teilzeichenfolgen in TypeScript zu extrahieren, verwenden wir die Methoden substr() und substring(). Gehen wir durch jeweils ein Beispiel für beide:

```TypeScript
let text: string = "Ich liebe TypeScript";
let subText1: string = text.substr(7, 9);  // "liebe Type"
console.log(subText1);  // Ausgabe: "liebe Type"

let subText2: string = text.substring(7, 16); // "liebe Type"
console.log(subText2);  // Ausgabe: "liebe Type"
```

Mit der Methode substr() geben wir die Anfangsposition und die Länge des Teilstrings an. Mit substring() geben wir Anfangs- und Endposition an.

## Vertiefung

Die Extraktion von Teilstrings ist eine wichtige Aufgabe in der Programmierung. Historisch gesehen ermöglichte sie die Verarbeitung großer Datenmengen, bevor höhere Abstraktionen wie Objekte und Klassen verfügbar waren.

Alternativen zu substr() und substring() sind die Methoden slice() und split(). slice() ist ähnlich wie substring() und split() teilt eine Zeichenkette anhand eines angegebenen Trennzeichens in einem Array auf.

Die Implementierung der Extraktion von Teilstrings in TypeScript basiert auf der von JavaScript, da TypeScript eine Obermenge von JavaScript ist. JavaScript behandelt Zeichenketten als Zeichenarrays, was die Extraktion von Teilzeichenketten ermöglicht.

## Siehe auch

Für weitere Details über das Arbeiten mit Zeichenketten in TypeScript, schauen Sie sich bitte die folgenden Quellen an:

[TypeScript: Dokumentation](https://www.typescriptlang.org/docs/)
[TypeScript: Zeichenketten und Zeichenkettenfunktionen](https://www.digitalocean.com/community/tutorials/typescript-strings-and-string-functions)