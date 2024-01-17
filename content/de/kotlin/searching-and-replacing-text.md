---
title:                "Textsuche und Ersetzung"
html_title:           "Kotlin: Textsuche und Ersetzung"
simple_title:         "Textsuche und Ersetzung"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Was & Warum?
Was ist die Suche und Ersetzung von Text? Wenn Sie Text in einem Dokument, einer Datei oder einer Quellcode-Datei suchen und durch einen anderen Text ersetzen, dann betreiben Sie die Suche und Ersetzung von Text. Programmierer nutzen diese Technik, um beispielsweise Fehler im Code zu beheben oder bestimmte Wörter oder Ausdrücke durch andere zu ersetzen.

# Wie geht's:
Hier sind zwei Beispiele, wie Sie die Suche und Ersetzung von Text in Kotlin durchführen können:

```
// Beispiel 1: Ersetzen aller Vorkommen von "Hund" durch "Katze" in einem String
val text = "Ich mag Hunde, aber meine Schwester mag Katzen."
val newText = text.replace("Hund", "Katze")
println(newText) // Ausgabe: "Ich mag Katzen, aber meine Schwester mag Katzen."

// Beispiel 2: Ersetzen aller Zahlen durch "X" in einem String
val text = "123 ist eine Zahl."
val newText = text.replace("\\d+".toRegex(), "X")
println(newText) // Ausgabe: "X ist eine Zahl."
```

# Tief einsteigen:
Die Suche und Ersetzung von Text ist eine weit verbreitete Technik, die in vielen Programmen und Anwendungen verwendet wird. Sie wurde erstmals in den 1960er Jahren in der Programmiersprache SNOBOL eingeführt und ist seitdem ein unverzichtbares Werkzeug für Softwareentwickler. Es gibt auch alternative Ansätze zur Suche und Ersetzung von Text, wie beispielsweise die Verwendung von regulären Ausdrücken oder speziellen Such- und Ersetzungsfunktionen in Texteditoren. In Kotlin wird die Funktion replace verwendet, um die Suche und Ersetzung von Text durchzuführen. Es können auch reguläre Ausdrücke oder andere Such- und Ersetzungsmethoden verwendet werden.

# Siehe auch:
- [Kotlin Dokumentation zu Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Online Regex Tester für Kotlin](https://regex101.com/r/1jzHpL/1)