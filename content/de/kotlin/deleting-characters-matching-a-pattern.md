---
title:                "Musterübereinstimmende Zeichen löschen"
html_title:           "Kotlin: Musterübereinstimmende Zeichen löschen"
simple_title:         "Musterübereinstimmende Zeichen löschen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine häufige Aufgabe bei der Softwareentwicklung. Programmierer tun dies, um unerwünschte oder unnötige Zeichen aus einem String zu entfernen und die Datenstruktur zu bereinigen.

## Wie geht das?
Es gibt verschiedene Möglichkeiten, Zeichen zu löschen, die einem bestimmten Muster entsprechen. Eine Möglichkeit ist die Verwendung von regulären Ausdrücken, die in Kotlin mit der Regex-Klasse umgesetzt werden können. Hier ist ein Beispiel:

```
fun main() {
    val str = "Hello World!"
    val pattern = Regex("[^A-Za-z ]")
    val result = str.replace(pattern, "")
    println(result) // Output: Hello World
}
```

In diesem Beispiel wird die Regex-Klasse verwendet, um alles außer Groß- und Kleinbuchstaben sowie Leerzeichen aus dem String zu entfernen. Der resultierende String wird dann ausgegeben, wobei alle unerwünschten Zeichen gelöscht wurden.

Eine andere Möglichkeit ist die Verwendung der Kotlin-Standardbibliotheksfunktion `filter`, die eine Lambda-Funktion als Parameter akzeptiert. Hier ist ein Beispiel:

```
fun main() {
    val str = "Hello World!"
    val result = str.filter { it.isLetter() || it.isWhitespace() }
    println(result) // Output: Hello World
}
```

In diesem Beispiel wird die `filter`-Funktion verwendet, um nur Zeichen zuzulassen, die Buchstaben oder Leerzeichen sind. Alle anderen Zeichen werden automatisch gelöscht.

## Tiefergehende Informationen
Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine Aufgabe, die bereits seit den Anfängen der Programmierung existiert. Früher wurde dies oft durch das Iterieren über den String und das Überprüfen jedes einzelnen Zeichens durchgeführt. Mit der Einführung von regulären Ausdrücken wurde diese Aufgabe jedoch vereinfacht und effizienter gestaltet.

Es gibt auch Alternativen zu regulären Ausdrücken, wie z.B. die Verwendung von String-Manipulationsmethoden wie `replace` oder `replaceAll`. Diese können jedoch unpraktisch und aufwändig sein, wenn es um komplexe Muster geht.

Die Implementierung von regulären Ausdrücken in Kotlin erfolgt über die `Regex`-Klasse, die viele nützliche Methoden wie `replace`, `matches` und `find` bietet. Es gibt auch eine Vielzahl von speziellen Zeichen und Metazeichen, die beim Erstellen von regulären Ausdrücken verwendet werden können, um bestimmte Muster anzugeben.

## Weitere Informationen
- Offizielle Kotlin-Dokumentation zu regulären Ausdrücken: https://kotlinlang.org/docs/regular-expressions.html
- Tutorial zu regulären Ausdrücken in Kotlin: https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm
- Weitere Informationen zu String-Manipulationsmethoden in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/