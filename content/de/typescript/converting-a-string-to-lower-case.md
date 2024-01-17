---
title:                "Eine Zeichenfolge in Kleinbuchstaben umwandeln."
html_title:           "TypeScript: Eine Zeichenfolge in Kleinbuchstaben umwandeln."
simple_title:         "Eine Zeichenfolge in Kleinbuchstaben umwandeln."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln eines Strings in Kleinbuchstaben ist eine gängige Aufgabe für Programmierer, die es ermöglicht, Texte in einheitlicher Schreibweise zu verarbeiten. Durch die Verwendung von Kleinbuchstaben können auch Vergleiche zwischen verschiedenen Strings erleichtert werden, da die Groß- und Kleinschreibung ignoriert wird.

## Wie geht's:
### TypeScript Beispiel:
```
const text: string = "Die SONNE scheint"; 
console.log(text.toLowerCase());
```
### Ausgabe:
```
die sonne scheint
```

## Vertiefung:
Um Strings in Kleinbuchstaben umzuwandeln, gibt es verschiedene Ansätze und Funktionen, die in vielen Programmiersprachen verwendet werden. Die Notwendigkeit, alle Buchstaben in einen einheitlichen Fall zu bringen, stammt aus den frühen Tagen der Computersysteme, als die erstellten Codes noch nicht zwischen Groß- und Kleinschreibung unterscheiden konnten. Als Alternative zur ```toLowerCase()``` Funktion gibt es in TypeScript auch die Möglichkeit, ```string.prototype.toLocaleLowerCase()``` zu verwenden, um spezifischere sprachspezifische Regeln für die Umwandlung anzuwenden.

## Siehe auch:
Weitere Informationen zum Thema Strings in TypeScript finden Sie in der offiziellen Dokumentation von TypeScript: https://www.typescriptlang.org/docs/handbook/strings.html und in diesem nützlichen Artikel über String-Manipulation: https://www.zenva.com/blog/string-manipulation-typescript/