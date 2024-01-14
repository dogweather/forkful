---
title:    "Kotlin: Untersuchen von Teilzeichenketten"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist in der Programmierung oft notwendig, um bestimmte Informationen aus längeren Texten zu erhalten. Dies kann nützlich sein, um beispielsweise Textfilter, Datenbankabfragen oder Suchfunktionen zu erstellen.

## Wie man Teilstrings extrahiert

Um Teilstrings in Kotlin zu extrahieren, gibt es zwei grundlegende Methoden: die `substring()` und `subSequence()` Funktionen. Diese Funktionen akzeptieren beide zwei Parameter: den Startindex und den Endindex des gewünschten Teilstrings.

Beispielcode mit Ausgabe:

```Kotlin
val text = "Dies ist ein Beispieltext."

val substring = text.substring(5, 11)
println(substring) // Ausgabe: "ist ein"
```

```Kotlin
val text = "Dies ist ein Beispieltext."

val subsequence = text.subSequence(5, 11)
println(subsequence) // Ausgabe: "ist ein"
```

Es ist auch möglich, nur den Startindex anzugeben und damit den Teilstring bis zum Ende des Textes zu extrahieren:

Beispielcode mit Ausgabe:

```Kotlin
val text = "Dies ist ein Beispieltext."

val substring = text.substring(5)
println(substring) // Ausgabe: "ist ein Beispieltext."
```

```Kotlin
val text = "Dies ist ein Beispieltext."

val subsequence = text.subSequence(5)
println(subsequence) // Ausgabe: "ist ein Beispieltext."
```

## Tiefergehender Einblick

Beim Extrahieren von Teilstrings gibt es einige wichtige Dinge zu beachten. Zum einen können die angegebenen Indizes nicht außerhalb des Textbereichs liegen, sonst gibt es einen `IndexOutOfBoundsException`. Außerdem gibt es noch die Möglichkeit, negative Indizes anzugeben. In diesem Fall werden die Zeichen vom Ende des Textes gezählt.

Beispielcode mit Ausgabe:

```Kotlin
val text = "Dies ist ein Beispieltext."

val substring = text.substring(5, -3)
println(substring) // Ausgabe: "ist ein Beispiel"
```

```Kotlin
val text = "Dies ist ein Beispieltext."

val subsequence = text.subSequence(-14, -8)
println(subsequence) // Ausgabe: "Beispiel"
```

Außerdem gibt es noch weitere Methoden, um Teilstrings zu extrahieren, wie z.B. `substringBefore()` und `substringAfter()`, um den Teilstring vor oder nach einem bestimmten Zeichen zu extrahieren.

## Siehe auch

- [Offizielle Dokumentation zu Strings in Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial: Strings in Kotlin](https://www.baeldung.com/kotlin/strings)
- [Java StringBuilder vs Kotlin String](https://stackoverflow.com/questions/48307883/stringbuilder-vs-string-in-kotlin-performance)