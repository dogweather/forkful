---
title:                "Teilstrings extrahieren"
aliases:
- /de/kotlin/extracting-substrings/
date:                  2024-01-20T17:46:06.719912-07:00
model:                 gpt-4-1106-preview
simple_title:         "Teilstrings extrahieren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teil-Strings ermöglicht es, spezifische Segmente aus einem größeren String herauszuschneiden. Programmierer nutzen dies, um Daten zu verarbeiten, Texte zu analysieren oder Formatierungen anzupassen.

## So geht's:
```kotlin
fun main() {
    val text = "Hallo, Kotlin-Entwickler!"
    val begruessung = text.substring(0, 5) // "Hallo"
    val berufsbezeichnung = text.substring(7, text.length) // "Kotlin-Entwickler!"

    println(begruessung) // Gibt "Hallo" aus
    println(berufsbezeichnung) // Gibt "Kotlin-Entwickler!" aus
}
```

## Vertiefung:
Das Extrahieren von Teil-Strings ist eine grundlegende Funktion vieler Programmiersprachen und schon lange vor Kotlin existent gewesen. Kotlin bietet verschiedene Methoden, wie `substring()` und `take()`, die je nach Bedarf eingesetzt werden können. Die `substring`-Methode arbeitet mit einem Indexbereich oder expliziten Start- und Endindizes, während `take()` und `takeLast()` eine bestimmte Anzahl von Zeichen vom Anfang oder Ende entnehmen. Ein besonderes Augenmerk sollte auf die String-Indices gelegt werden, da Kotlin, wie viele moderne Sprachen, bei 0 zu zählen beginnt und der Endindex exklusiv ist.

## Siehe auch:
- Kotlin Dokumentation zu Strings: [https://kotlinlang.org/docs/strings.html](https://kotlinlang.org/docs/strings.html)
- Kotlin API Referenz zu `substring`: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
