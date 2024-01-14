---
title:    "Kotlin: Großschreibung eines Strings"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele verschiedene Gründe, warum Sie sich möglicherweise mit der Großschreibung von Strings beschäftigen wollen. Vielleicht müssen Sie Benutzereingaben standardisieren oder einen Titel in der richtigen Schreibweise anzeigen. Egal aus welchem Grund, das Kapitalisieren eines Strings ist eine nützliche Fähigkeit für jeden Programmierer.

## Wie man es macht

Um einen String in Kotlin zu kapitalisieren, können Sie die `capitalize()`-Funktion verwenden. Sie akzeptiert keine Parameter und gibt den String in Großbuchstaben zurück.

````Kotlin
val name = "Max Mustermann"
println(name.capitalize())

// Output: Max mustermann
````

Sie können auch die `toUpperCase()`-Funktion verwenden, um alle Buchstaben in einem String in Großbuchstaben umzuwandeln.

````Kotlin
val name = "Max Mustermann"
println(name.toUpperCase())

// Output: MAX MUSTERMANN
````

Es ist auch möglich, nur den ersten Buchstaben eines Strings zu verändern und alle anderen Buchstaben in Kleinbuchstaben zu belassen. Hierfür können Sie die `capitalizeFirst()`-Funktion verwenden.

````Kotlin
val name = "max mustermann"
println(name.capitalizeFirst())

// Output: Max mustermann
````

## Tiefere Einblicke

Bei der Verwendung von `capitalize()` oder `toUpperCase()` ist es wichtig zu beachten, dass diese Funktionen nur ASCII-Zeichen umwandeln. Wenn Sie jedoch mit nicht-ASCII-Zeichen arbeiten möchten, sollten Sie die `toUpperCase(Locale)`-Funktion verwenden und eine bestimmte Sprachregion angeben, um eine korrekte Transformation zu gewährleisten.

Sie können auch selbst eine Funktion schreiben, die jedes Wort in einem String kapitalisiert. Hier ist eine Beispielimplementierung:

````Kotlin
fun String.capitalizeWords(): String = split(" ").joinToString(" ") { it.capitalize() }
````

## Siehe auch

- [String capitalization in Kotlin](https://www.baeldung.com/kotlin/capitalize-string)
- [Official Kotlin documentation on strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Working with strings in Kotlin](https://blog.mindorks.com/working-with-strings-in-kotlin)