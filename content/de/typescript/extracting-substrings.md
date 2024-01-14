---
title:    "TypeScript: Extrahieren von Teilstrings"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum substrings extrahiert werden sollten

Substrings (Unterzeichenketten) sind ein wichtiges Konzept in der Programmierung und werden oft in der täglichen Arbeit mit TypeScript verwendet. Sie bestehen aus Teilen einer größeren Zeichenkette und können sehr nützlich sein, um spezifische Informationen aus längeren Daten zu extrahieren. In diesem Blogbeitrag werden wir uns genauer mit dem Extrahieren von Substrings in TypeScript beschäftigen und warum dies eine wichtige Fähigkeit für jeden TS-Entwickler ist.

## Wie man Substrings in TypeScript extrahiert

Um Substrings in TypeScript zu extrahieren, gibt es mehrere Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung der `substr()`-Methode, die auf einer Zeichenkette aufgerufen werden kann. Diese Methode nimmt zwei Argumente entgegen: den Startindex und die Anzahl der zu extrahierenden Zeichen.

```TypeScript
let str: string = "Dies ist ein Beispieltext.";
let substring: string = str.substr(5, 4); // substring enthält "ist "
```

Man kann auch die `substring()`-Methode verwenden, die ähnlich funktioniert, jedoch den Startindex und den Endindex anstelle der Anzahl der Zeichen als Argumente verwendet.

```TypeScript
let str: string = "Dies ist ein Beispieltext.";
let substring: string = str.substring(5, 7); // substring enthält "ist ein"
```

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken, um spezifische Muster in einer Zeichenkette zu suchen und zu extrahieren.

```TypeScript
let str: string = "Dies ist eine Beispielnummer: 12345.";
let regex: RegExp = /\d{5}/; // sucht nach einer Folge von fünf Zahlen
let substring: string = str.match(regex)[0]; // substring enthält "12345"
```

Die Ausgabe der obigen Beispiele können je nach verwendeter Methode variieren, jedoch sollte sie immer ein Teil der ursprünglichen Zeichenkette sein.

## Tiefer Einblick in das Extrahieren von Substrings

Es gibt viele Situationen, in denen das Extrahieren von Substrings hilfreich sein kann. Zum Beispiel, wenn man Daten aus einer API-Antwort analysiert oder wenn man Benutzereingaben auf spezifische Muster überprüft. Reguläre Ausdrücke sind besonders nützlich, da sie es ermöglichen, komplexe Muster in einer Zeichenkette zu suchen und zu extrahieren.

Ein wichtiger Aspekt beim Extrahieren von Substrings ist auch die Beachtung von unterschiedlichen Zeichenkodierungen, insbesondere bei der Verwendung von Sonderzeichen oder bei mehrsprachigen Anwendungen. Es ist wichtig sicherzustellen, dass die verwendeten Methoden korrekt die gewünschten Substrings extrahieren, unabhängig von der verwendeten Zeichenkodierung.

## Siehe auch

- Die [offizielle TypeScript-Dokumentation](https://www.typescriptlang.org/docs) für weitere Informationen zur Verwendung von Substrings und anderen Zeichenkettenfunktionen
- Eine [Einführung in reguläre Ausdrücke in TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-typescript) für weitere Details und Beispiele zur Verwendung von Regex mit Substrings
- Ein [Codebeispiel auf StackOverflow](https://stackoverflow.com/questions/7936787/how-to-extract-a-substring-between-two-strings-in-javascript) zum Extrahieren eines Substrings zwischen zwei bestimmten Zeichen in JavaScript. Dieses kann leicht auf TypeScript angepasst werden.