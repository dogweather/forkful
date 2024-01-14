---
title:    "Kotlin: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Entfernen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Technik in der Programmierung. Es kann verwendet werden, um unerwünschte Zeichen aus einer Zeichenkette zu entfernen oder um bestimmte Teile einer Zeichenkette zu formatieren.

## How To
Hier ist ein Beispiel, wie man in Kotlin Zeichen löscht, die einer bestimmten Bedingung entsprechen:

```Kotlin
fun main() {
    val input = "Diese Zeichen sind alle unnötig!#$% "
    
    // Entferne alle Zeichen, die nicht Buchstaben oder Zahlen sind
    val output = input.filter { it.isLetterOrDigit() }
    
    println(output)
}
```

Output: `DieseZeichensindalleunötig`

Dies ist nur ein einfaches Beispiel, wie die `filter`-Funktion verwendet werden kann, um Zeichen zu entfernen. Es gibt viele verschiedene Möglichkeiten, wie dies in der Praxis angewendet werden kann.

### Noch tiefer eintauchen
Wenn Sie tiefer in das Entfernen von Zeichen in Kotlin einsteigen möchten, können Sie die `regex`-Bibliothek verwenden, um ein reguläres Ausdrucksmuster zu verwenden, anstatt nur eine einfache Bedingung. Mit dem regulären Ausdruck können Sie dann noch spezifischere Zeichen löschen. Hier ein Beispiel:

```Kotlin
fun main() {
    val input = "Dieser Text enthält <b>HTML-Tags</b> wie <span>diesen</span>."
    
    // Entferne alle HTML-Tags aus dem Eingabetext
    val output = input.replace(Regex("<.*?>"), "")
    
    println(output)
}
```

Output: `Dieser Text enthält HTML-Tags wie diesen.`

In diesem Beispiel haben wir die `replace`-Funktion verwendet, um alle Zeichen, die zwischen `<` und `>` stehen, zu löschen. Dies zeigt, wie leistungsstark reguläre Ausdrücke sein können, wenn es darum geht, bestimmte Zeichen zu entfernen.

## Siehe auch
- [Kotlin-Dokumentation: filter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/filter.html)
- [Kotlin-Dokumentation: regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/index.html)
- [Java-Tutorial: reguläre Ausdrücke](https://docs.oracle.com/javase/tutorial/essential/regex/)