---
title:                "Verwendung regulärer Ausdrücke"
html_title:           "TypeScript: Verwendung regulärer Ausdrücke"
simple_title:         "Verwendung regulärer Ausdrücke"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke sind eine Methode zum Durchsuchen und Manipulieren von Texten in Programmiersprachen wie TypeScript. Programmierer nutzen reguläre Ausdrücke, um komplexe Suchanfragen zu vereinfachen oder bestimmte Teile eines Textes zu extrahieren.

## Wie geht's?

Die Verwendung von regulären Ausdrücken beginnt immer mit der Deklaration der Eingabezeichenkette. Dabei können verschiedene Methoden angewendet werden, um Muster in der Zeichenkette zu erkennen und zu verarbeiten.

Ein Beispiel für die Suche nach einer bestimmten Zeichenfolge in einer Zeichenkette:

```TypeScript
const string = "Hallo Welt!";
const pattern = /Welt/;
const result = string.match(pattern);

console.log(result); // Ausgabe: ["Welt"]
```

Andere Methoden, wie z.B. `replace()` oder `test()`, können auch verwendet werden, um Texte zu manipulieren oder Muster zu überprüfen.

## Tiefentauchen

Reguläre Ausdrücke wurden erstmals in den 1950er Jahren von dem Mathematiker Stephen Kleene eingeführt. Seitdem haben sie sich zu einem wichtigen Bestandteil der Programmierung entwickelt und werden von Entwicklern häufig bei der Validierung von Benutzereingaben oder beim Scraping von Daten verwendet.

Als Alternative zu regulären Ausdrücken können auch String-Methoden wie `indexOf()` oder `substring()` verwendet werden, aber diese sind oft nicht so leistungsfähig und flexibel wie reguläre Ausdrücke.

Bei der Implementierung von regulären Ausdrücken in TypeScript ist es wichtig zu beachten, dass sie case sensitive sind und spezielle Zeichen wie ".", "*", oder "?" beachtet werden müssen.

## Siehe auch

Weitere Informationen zu regulären Ausdrücken in TypeScript und eine Übersicht über verfügbare Methoden finden Sie in der offiziellen TypeScript-Dokumentation unter https://www.typescriptlang.org/docs/handbook/regular-expressions.html.