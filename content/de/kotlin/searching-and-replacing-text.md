---
title:                "Kotlin: Suchen und Ersetzen von Text"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum
Textsuche und -ersetzung (engl. searching and replacing) zu beherrschen ist essentiell für jeden, der mit Programmierung zu tun hat. Diese Fähigkeit ermöglicht es, effizienter und schneller Code zu schreiben und Fehler zu beheben.

## Wie geht's
Die grundlegenden Schritte für Textsuche und -ersetzung in Kotlin sind einfach:

1. Importiere die `text` Bibliothek von Kotlin.
2. Verwende den Befehl `replace()` um die gewünschten Textstellen zu ersetzen.
3. Optional: Verwende den Befehl `find()` um zu überprüfen, ob ein bestimmter Text vorhanden ist.

Hier ist ein Beispiel, um das Wort "Hallo" in einem String durch "Hallo Welt" zu ersetzen:

```Kotlin
import kotlin.text.*

fun main() {
    val text = "Hallo Kotlin!"
    val newText = text.replace("Kotlin", "Welt")
    println(newText)
    
    // Output: Hallo Welt!
}
```

## Einblicken
Textsuche und -ersetzung kann auch komplexere Muster beinhalten, anstatt nur einfache Textersetzung. Dafür bietet Kotlin die `Regex` Klasse an, mit der man reguläre Ausdrücke nutzen kann.

Hier ist ein Beispiel, um alle Zahlen in einem String durch "x" zu ersetzen:

```Kotlin
import kotlin.text.*

fun main() {
    val text = "1234ab5678"
    val pattern = Regex("[0-9]")
    val newText = pattern.replace(text, "x")
    println(newText)
    
    // Output: xxxxabxxxx
}
```

Reguläre Ausdrücke können auch verwendet werden, um bestimmte Muster in einem Text zu finden und zu manipulieren, was die Textsuche und -ersetzung noch mächtiger macht.

## Siehe auch
- [Kotlin Standardbibliothek](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- [Reguläre Ausdrücke in Kotlin](https://kotlinlang.org/docs/regex.html)
- [Textsuche und -ersetzung in Java](https://docs.oracle.com/javase/tutorial/essential/regex/)