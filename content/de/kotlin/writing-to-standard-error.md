---
title:                "Kotlin: Schreiben auf die Standardfehler"
simple_title:         "Schreiben auf die Standardfehler"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf die Standard error Konsole ist ein wichtiger Aspekt der Programmierung. Es ermöglicht es uns, Fehler und Warnungen in unserem Code zu identifizieren und zu beheben. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man in Kotlin effektiv auf die Standard error Konsole schreibt und warum dies so wichtig ist.

## Wie man auf die Standard error Konsole schreibt:

Um in Kotlin auf die Standard error Konsole zu schreiben, verwenden wir die Funktion "System.err.println()". Diese Funktion akzeptiert einen beliebigen String als Argument und gibt ihn dann auf der Standard error Konsole aus. Schauen wir uns ein Beispiel an:

```Kotlin
fun main() {
    System.err.println("Dies ist ein Beispiel")
}
```

Die Ausgabe dieses Codes wird auf der Standard error Konsole erscheinen:

`Dies ist ein Beispiel`

Dies hilft uns dabei, unsere Probleme im Code zu identifizieren und zu beheben. Wir können auch Variablen innerhalb der Print-Anweisung verwenden, um detailliertere Informationen zu erhalten. Schauen wir uns ein weiteres Beispiel an:

```Kotlin
fun main() {
    val a = 10
    val b = 5
    System.err.println("Die Summe von $a und $b ist ${a + b}")
}
```

Dieses Beispiel gibt die folgende Ausgabe auf der Standard error Konsole aus:

`Die Summe von 10 und 5 ist 15`

Wir können also sehen, dass das Schreiben auf die Standard error Konsole uns hilft, unsere Probleme effektiv und schnell zu finden.

## Tiefergehende Informationen:

Es gibt auch andere Funktionen, die wir verwenden können, um auf die Standard error Konsole in Kotlin zu schreiben, wie z.B. "System.err.print()", "System.err.printf()" oder "System.err.format()". Diese Funktionen ermöglichen es uns, unsere Ausgabe zu formatieren und verschiedene Datentypen zu verarbeiten. Sie bieten auch die Möglichkeit, die Ausgabe in eine Datei umzuleiten, um sie später zu überprüfen.

Es ist auch wichtig zu beachten, dass die Standard error Konsole für die Ausgabe von Fehlern und Warnungen gedacht ist und nicht für die normale Programmausgabe. Für die normale Ausgabe sollten wir die Funktion "System.out.println()" verwenden.

Insgesamt ist das Schreiben auf die Standard error Konsole eine wichtige Fähigkeit, die uns dabei hilft, unseren Code effizient und effektiv zu debuggen und zu verbessern.

## Siehe auch:

- [Offizielle Kotlin Dokumentation](https://kotlinlang.org/docs/reference/basic-syntax.html)
- [Tutorial zu Standard error in Kotlin auf YouTube](https://www.youtube.com/watch?v=H063mgVKrDk)
- [Tutorial zu Standard error in Kotlin auf Open Source Learning](https://learnbyexample.github.io/Kotlin-Snippets/error_output.html)