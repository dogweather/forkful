---
title:                "Kotlin: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings ist eine häufig verwendete Funktion in der Programmierung. Sie kann verwendet werden, um Texte in einem bestimmten Format anzuzeigen oder um bestimmte Wörter hervorzuheben. In diesem Blog-Post werden wir uns ansehen, wie man Strings in Kotlin kapitalisiert und einige tiefere Einblicke in die Funktionsweise gibt.

## Wie man Strings in Kotlin kapitalisiert

```Kotlin
// Beispiel eines Strings
val text = "hallo welt"

// Verwenden der capitalize() Funktion
val capitalizedText = text.capitalize()

// Ausgabe des kapitalisierten Textes
println(capitalizedText)

// Output: Hallo welt
```

In diesem Beispiel haben wir einen String mit dem Text "hallo welt" definiert. Dann verwenden wir die eingebaute Funktion `capitalize()`, um den Text zu kapitalisieren. Diese Funktion konvertiert den ersten Buchstaben des Strings in einen Großbuchstaben und lässt alle anderen Buchstaben unverändert. Die Ausgabe des obigen Codes wäre "Hallo welt".

Natürlich gibt es noch weitere Möglichkeiten, um Strings zu kapitalisieren. Die `toUpperCase()` Funktion zum Beispiel wandelt den gesamten String in Großbuchstaben um, während `toLowerCase()` alle Buchstaben in Kleinbuchstaben konvertiert.

```Kotlin
// Beispiel eines Strings
val text = "hallo welt"

// Verwenden der toUpperCase() Funktion
val uppercaseText = text.toUpperCase()

// Ausgabe des Strings in Großbuchstaben
println(uppercaseText)

// Output: HALLO WELT
```

## Tiefer Einblick

Beim Kapitalisieren von Strings gibt es einige wichtige Dinge zu beachten. Zum Beispiel behandeln manche Sprachen bestimmte Buchstaben anders, wenn sie in Großbuchstaben umgewandelt werden. In diesen Fällen kann es sinnvoll sein, eine spezielle Funktion zu verwenden, die diese Unterschiede berücksichtigt. In Kotlin gibt es dafür die Funktion `toLocaleUpperCase()`, die sprachspezifische Großbuchstaben erzeugt.

Ein weiteres wichtiges Konzept ist die Behandlung von Leerzeichen innerhalb eines Strings. Bei der Funktion `capitalize()` wird nur der erste Buchstabe des Strings in Großbuchstaben umgewandelt. Wenn innerhalb des Strings jedoch Leerzeichen vorhanden sind, bleibt der Buchstabe direkt nach dem Leerzeichen klein. Dies kann zu unerwarteten Ergebnissen führen, wenn man Strings mit mehreren Wörtern kapitalisieren möchte. In solchen Fällen kann es hilfreich sein, eine spezielle Funktion zu nutzen, die alle Wörter in einem String großschreibt.

## Siehe auch

- [Kotlin Strings](https://kotlinlang.org/docs/basic-types.html#strings)
- [String Functions in Kotlin](https://medium.com/the-kotlin-coding-club/string-functions-in-kotlin-d1f8b53c01c0)
- [Konvertieren von Strings in Kotlin](https://www.geeksforgeeks.org/kotlin-convert-string-uppercase-lowercase/)