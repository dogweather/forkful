---
title:                "Extrahieren von Teilzeichenketten"
html_title:           "Kotlin: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was und warum?

Die Extraktion von Teilstrings, auch bekannt als Substring-Extraktion, ist ein häufig verwendetes Konzept bei der Programmierung. Dabei werden Teile von Texten oder Zeichenketten extrahiert, die benötigt werden, während der Rest ignoriert wird. Dies kann nützlich sein, um bestimmte Informationen aus längeren Texten herauszufiltern oder um Texte in kleinere Einheiten aufzuteilen.

## Wie geht's?

Um Teilstrings in Kotlin zu extrahieren, kannst du die `substring()` Methode verwenden. Diese Methode hat zwei Parameter: den Startindex, an dem die Extraktion beginnen soll, und den endindex, an dem die Extraktion endet. Beide Indizes beziehen sich auf die Positionen der Zeichen in der Zeichenkette.

Beispiel:
```Kotlin
val text = "Hallo Welt"
val sub = text.substring(0, 5) // "Hallo"
```

Der Startindex wird dabei inklusive der entsprechenden Zeichen der Zeichenkette enthalten, während der endindex exklusive der entsprechenden Zeichen ist.

## Tiefes Eintauchen

Die Extraktion von Teilstrings ist in vielen Programmiersprachen eine gebräuchliche Technik und ermöglicht es Entwicklern, Texte auf effiziente Weise zu bearbeiten. Eine alternative Methode in Kotlin wäre die Verwendung von regulären Ausdrücken (RegEx), die noch mehr Kontrolle über die Extraktion bieten können, aber auch komplexer zu verwenden sind.

## Siehe auch

- [Kotlin String API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [Java String substring() Method](https://www.w3schools.com/javaref/met_string_substring.asp) (ähnlicher Ansatz wie in Kotlin)
- [Regular Expressions in Kotlin](https://kotlinlang.org/docs/regular-expressions.html)