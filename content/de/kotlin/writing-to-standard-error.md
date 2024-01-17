---
title:                "Schreiben auf Standardfehler"
html_title:           "Kotlin: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben auf der Standardfehlerausgabe ist eine Praxis unter Programmierern, um Fehler und andere wichtige Informationen während der Ausführung von Programmen zu drucken. Diese Ausgabe ist unabhängig von der Standardausgabe, ermöglicht es Entwicklern aber, kritische Informationen zu verfolgen, die nicht zur normalen Ausgabe gehören. Es ist eine effektive Möglichkeit, Probleme während der Entwicklung oder Tests zu erkennen.

## Wie geht's?

Um auf die Standardfehlerausgabe in Kotlin zu schreiben, verwenden Sie den Befehl ```System.err.println()```. Dieser Befehl gibt die angegebene Nachricht auf der Standardfehlerausgabe aus. Hier ist ein Beispielcode:

```Kotlin
fun main() {
    val num = 10
    if (num < 0) {
        System.err.println("Negative Zahlen sind nicht erlaubt.")
    }
}
```

Die Ausgabe wird folgendes sein:

```Error: Negative Zahlen sind nicht erlaubt.```

## Tiefer eintauchen

Das Schreiben auf der Standardfehlerausgabe hat seinen Ursprung im UNIX-Betriebssystem, das bereits in den 1970er Jahren existierte. Damals wurde es verwendet, um Fehler während der Ausführung von Programmen zu verfolgen. In der heutigen Zeit ist es eine gängige Praxis unter Entwicklern, da es eine einfache Möglichkeit ist, wichtige Informationen zu erkennen und zu debuggen.

Alternativ können Entwickler auch Logging-Frameworks wie Log4j verwenden, um auf die Standardfehlerausgabe zu schreiben. Diese Frameworks bieten zusätzliche Funktionen wie das Protokollieren von Ausnahmen und das Formatieren von Protokollmeldungen.

In Kotlin ist das Schreiben auf der Standardfehlerausgabe auch Teil des Kotlin-Standardbibliothek. Es gibt eine separate Klasse ```System.err```, die verwendet werden kann, um auf die Standardfehlerausgabe zu schreiben.

## Siehe auch

- [Kotlin Standard Library - System.err](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-system.err/)
- [Log4j](https://logging.apache.org/log4j/2.x/)