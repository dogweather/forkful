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

## Warum

Das Schreiben von Standardfehlermeldungen ist eine häufige Praxis in der Entwicklung mit Kotlin, um Fehler und Ausnahmen zu identifizieren. Oftmals werden diese Fehlermeldungen an den Benutzer zurückgegeben, um ihm hilfreiche Informationen bei der Fehlerbehebung zu bieten.

## Wie geht das?

Um eine Fehlermeldung oder Ausnahme an die Standardfehlerausgabe zu schreiben, kann die Funktion `System.err.println()` verwendet werden. Hier ist ein Beispiel, wie man dies in Kotlin machen kann:

```Kotlin
fun main() {
    try {
        val result = 10 / 0 // Division durch 0 auslösen
        println("Ergebnis: $result")
    } catch (e: ArithmeticException) {
        System.err.println("Fehler: " + e.message)
    }
}
```

Die Ausgabe des obigen Beispiels wäre:

```
Fehler: / by zero
```

Durch die Verwendung von `System.err.println()` wird sichergestellt, dass die Fehlermeldung an die Standardfehlerausgabe geschrieben wird und nicht an die Standardausgabe.

## Tiefere Einblicke

Wenn ein Programm Fehler oder Ausnahmen hat, ist es wichtig, diese Informationen an den Benutzer weiterzugeben, damit er die Möglichkeit hat, den Fehler zu verstehen und zu beheben. Das Schreiben von Informationen an die Standardfehlerausgabe ist eine effektive Methode, um dies zu erreichen. Es ist auch wichtig zu beachten, dass das Schreiben von zu vielen Informationen an die Standardfehlerausgabe zu unübersichtlichen Ausgaben führen kann, daher sollte dies mit Bedacht verwendet werden.

## Siehe auch

- [Kotlin Dokumentation zur Standardfehlerausgabe](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-print-stream/write-error.html)
- [Artikel über Fehlerbehandlung in Kotlin](https://www.baeldung.com/kotlin/try-catch)
- [Vergleich von Standard- und Fehlerausgabe in Kotlin](https://www.techiedelight.com/differences-standard-error-output-kotlin/)