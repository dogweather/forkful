---
title:                "Kotlin: Eine Zeichenfolge in Kleinbuchstaben umwandeln."
simple_title:         "Eine Zeichenfolge in Kleinbuchstaben umwandeln."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Wenn Sie bereits mit der Programmiersprache Kotlin vertraut sind, haben Sie möglicherweise schon einmal die Funktion gesehen, mit der Sie einen String in Kleinbuchstaben umwandeln können. Aber warum sollte man das überhaupt tun? Die Antwort ist einfach: Es erleichtert die Verarbeitung und Vergleichung von Strings. Indem alle Buchstaben in Kleinbuchstaben umgewandelt werden, wird die Vergleichung vereinfacht und mögliche Unterschiede aufgrund von Groß- und Kleinschreibung eliminiert.

## Wie man Strings in Kleinbuchstaben umwandelt
Es gibt mehrere Möglichkeiten, einen String in Kotlin in Kleinbuchstaben umzuwandeln. Hier sind zwei Beispiele mit ihren jeweiligen Ausgaben:

```Kotlin
val string = "Hallo Welt"
val lowercaseString = string.toLowerCase()
println(lowercaseString)
// Ausgabe: hallo welt
```

```Kotlin
val string = "Ich liebe Kotlin"
val uppercaseString = string.toUpperCase()
println(uppercaseString)
// Ausgabe: ICH LIEBE KOTLIN
```

Wie Sie sehen können, gibt es für die Umwandlung eines Strings in Kleinbuchstaben die Funktion `toLowerCase()`, während für die Umwandlung in Großbuchstaben die Funktion `toUpperCase()` verwendet werden kann.

## Tiefgehende Informationen
Es gibt jedoch mehr, als nur Strings in der gesamten Zeichenkette in Klein- oder Großbuchstaben umzuwandeln. Sie können auch bestimmte Bereiche der Zeichenkette auswählen und in Klein- oder Großbuchstaben ändern. Dazu können Sie die Funktion `substring()` in Kombination mit `toLowerCase()` oder `toUpperCase()` verwenden.

```Kotlin
val string = "Hallo Welt"
val substring = string.substring(0, 5).toUpperCase()
println(substring)
// Ausgabe: HALLO
```

In diesem Beispiel wird der Teil der Zeichenkette von 0 bis 5 (nicht inklusive) ausgewählt und in Großbuchstaben umgewandelt. Sie können auch andere Bereiche der Zeichenkette auswählen, je nachdem, was Sie benötigen.

## Siehe auch
- [Kotlin Strings und Zeichen](https://kotlinlang.org/docs/reference/basic-types.html#strings-and-characters) 
- [Offizielle Dokumentation von Kotlin zur Funktion "toLowerCase()"](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Wiki-Seite für die Funktion "toUpperCase()" in Kotlin](https://en.wikipedia.org/wiki/Case_conversion)