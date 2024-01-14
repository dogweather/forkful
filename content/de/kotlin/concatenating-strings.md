---
title:                "Kotlin: Verkettung von Zeichenfolgen"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verkettet von Strings ist eine häufig genutzte Funktion in der Programmierung. Es ermöglicht uns, mehrere Zeichenfolgen miteinander zu verbinden und so einen einzelnen Satz oder eine Nachricht zu erstellen. Dies ist besonders hilfreich, wenn mehrere Variablen oder Textbausteine zu einer Nachricht zusammengefügt werden müssen.

## Wie geht man vor

Um Strings in Kotlin zu verketten, können wir den Operator `+` verwenden. Ein Beispiel dafür wäre:

```Kotlin
val name = "Lisa"
val age = 25
val message = "Hallo, mein Name ist " + name + " und ich bin " + age + " Jahre alt."
println(message)
```

Dies würde folgende Ausgabe produzieren:

`Hallo, mein Name ist Lisa und ich bin 25 Jahre alt.`

Wir können auch den String-Interpolation-Syntax von Kotlin nutzen, um einfacher und effizienter zu verketten. Hier ein Beispiel:

```Kotlin
val name = "Peter"
val age = 30
val message = "Hallo, mein Name ist $name und ich bin $age Jahre alt."
println(message)
```

Dies würde ebenfalls die gleiche Ausgabe erzeugen:

`Hallo, mein Name ist Peter und ich bin 30 Jahre alt.`

## Tiefere Einblicke

Beim Verketten von Strings sollten wir darauf achten, die richtige Reihenfolge einzuhalten. Sie können in beliebiger Reihenfolge aufgerufen werden, aber es ist wichtig, die Variablen oder Textbausteine in der richtigen Reihenfolge anzugeben, damit die Nachricht sinnvoll bleibt.

Ein weiterer wichtiger Punkt ist, dass Strings in Kotlin unveränderlich sind, was bedeutet, dass sie nach der Erstellung nicht mehr geändert werden können. Aus diesem Grund können wir nicht direkt Zeichen zu einem bereits vorhandenen String hinzufügen, stattdessen müssen wir einen neuen String erstellen, der den bereits vorhandenen mit dem neuen Zeichen enthält.

## Siehe auch

- Offizielle Dokumentation zu Strings in Kotlin (https://kotlinlang.org/docs/reference/basic-types.html#strings)
- Ein Leitfaden zur Verwendung von String-Interpolation in Kotlin (https://www.baeldung.com/kotlin-string-interpolation)