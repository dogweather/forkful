---
title:                "Kotlin: Ein String großschreiben"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Capitalizing (Großschreibung) in Programmierung ist wichtig, um Texte lesbarer zu machen, Daten zu vergleichen oder für andere Prozesse zu nutzen.

## Wie funktioniert es?

Kotlin bietet verschiedene Möglichkeiten, um einen String zu verändern und den ersten Buchstaben großzuschreiben.

### Beispiel 1

```Kotlin
// Erstelle eine Beispiel-String Variable
var text: String = "hallo welt"

println(text.capitalize()) // Ausgabe: "Hallo welt"
```

In diesem Beispiel wird die `capitalize()` Funktion verwendet, um den ersten Buchstaben des Strings großzuschreiben. Es ist wichtig zu beachten, dass der ursprüngliche String nicht verändert wird, sondern dass die Funktion einen neuen String mit der Großschreibung zurückgibt.

### Beispiel 2

```Kotlin
// Erstelle eine Beispiel-String Variable
var text: String = "das ist ein beispiel"

// Erstelle eine Liste der Wörter im String
var words = text.split(" ")

// Verwende eine Schleife, um die Wörter in der Liste zu verändern
for (i in 0 until words.size) {
    words[i] = words[i].replaceFirstChar { it.uppercase() }
}

// Füge die Wörter aus der Liste wieder zu einem String zusammen
println(words.joinToString(" ")) // Ausgabe: "Das Ist Ein Beispiel"
```

In diesem Beispiel wird der String zuerst in eine Liste von Wörtern aufgeteilt und dann innerhalb einer Schleife jedes Wort verändert, um den ersten Buchstaben großzuschreiben. Am Ende wird die Liste wieder zu einem String zusammengefügt und ausgegeben.

## Tiefergehende Informationen

Es gibt auch die Funktion `capitalizeWords()`, die gleichzeitig alle Wörter in einem String großschreibt. Darüber hinaus gibt es auch Möglichkeiten, um nur den ersten Buchstaben eines Satzes oder von Eigennamen großzuschreiben.

Es ist auch wichtig zu beachten, dass die Großschreibung je nach Sprache unterschiedlich funktioniert. In manchen Sprachen ist es üblich, alle Wörter im Satz großzuschreiben, während in anderen nur der erste Buchstabe großgeschrieben wird. Es ist daher wichtig, die Anforderungen und Konventionen der jeweiligen Sprache zu beachten.

## Siehe auch

- [Kotlin String API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Offizielle Dokumentation über Strings in Kotlin](https://kotlinlang.org/docs/strings.html)