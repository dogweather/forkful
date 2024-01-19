---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

In der Programmierung führt die Umwandlung eines Strings in Kleinbuchstaben oft zu einer Verringerung von Fehlern. Warum? Weil diese Umwandlung dem Prozess der Normalisierung dient und dabei hilft, Inkonsistenzen in der Groß- und Kleinschreibung zu beseitigen, die sonst zu Verwechslungen führen könnten.

## Wie:

In Kotlin ist die String-Konvertierung in Kleinbuchstaben eine schnelle und einfache Aufgabe. Hier ist ein Beispiel:

```Kotlin
val gruss: String = "Hallo WELT"
val grussInKleinbuchstaben: String = gruss.lowercase()

println(gruss) // Ausgabe: Hallo WELT
println(grussInKleinbuchstaben) // Ausgabe: hallo welt
```
## Tiefer Einblick

Die Methode `lowercase()` in Kotlin ist eine bequeme Funktion, die in Kotlin 1.5.0 eingeführt wurde. Vor der Einführung dieser Methode, in älteren Versionen von Kotlin, wurde `toLowerCase()` verwendet. Die `lowercase()` Methode ist vorzuziehen, da sie den Unicode-Standard für die Konvertierung von Großbuchstaben in Kleinbuchstaben verwendet.

Eine mögliche Alternative zur Verwendung von `lowercase()`, insbesondere wenn Sie mit anderen Programmiersprachen arbeiten, ist die Verwendung von `toLowerCase()`. Allerdings ist die Verwendung von `lowercase()` ab Kotlin 1.5.0 vorzuziehen, da diese Methode internationalisierungssichere Konvertierungen bietet.

Bei der Implementierung von `lowercase()` unter der Haube verwendet Kotlin intern den `java.lang.String.toLowerCase` Aufruf des JVM, um sicherzustellen, dass die Konvertierung ordnungsgemäß durchgeführt wird.

## Siehe auch

Für detailliertere Informationen über die String-Konvertierung in Kotlin, hier sind ein paar Links zu relevanten Dokumenten und Tutorials:

- [Kotlin String Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/lowercase.html)
- [String in Kotlin: Grundlegende Operationen](https://www.programiz.com/kotlin-programming/string)
- [Kotlin Praxistutorial: Arbeiten mit Zeichenketten](https://www.tutorialkart.com/kotlin/kotlin-string-class-and-methods/)