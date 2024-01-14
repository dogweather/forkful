---
title:                "Kotlin: Schreiben für Standardfehler"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie schon einmal einen Kotlin-Code geschrieben haben, sind Sie wahrscheinlich auf die Funktion "standard error" gestoßen. Aber warum sollte man überhaupt diese Funktion nutzen? Nun, das einfache Schreiben zu Standardfehlern ermöglicht es Ihnen, Fehlermeldungen zu erfassen und zu verfolgen, was Ihrem Code beim Debuggen hilft.

## Wie geht das

Das Schreiben zu Standardfehlern in Kotlin ist sehr einfach und kann in nur wenigen Zeilen Code erreicht werden. Schauen wir uns ein Beispiel an:

```
fun main() {
    val number = 5
    try {
        val result = number / 0
    } catch(e: ArithmeticException) {
        System.err.println("Division durch 0 ist nicht erlaubt!")
    }
}
```

In diesem Beispiel definieren wir eine Variable "number", die den Wert 5 hat. Dann nutzen wir die "try-catch" Methode, um einen Fehler abzufangen, der entstehen könnte, wenn wir versuchen würden, "number" durch 0 zu teilen. Im "catch"-Abschnitt nutzen wir "System.err.println()" um eine Fehlermeldung zu erzeugen, die direkt zu standard error geschickt wird. Diese Meldung können wir dann nutzen, um den Fehler zu identifizieren und zu beheben.

## Tiefergehende Informationen

Das Schreiben zu Standardfehlern ist wichtig, weil es uns hilft, Probleme in unserem Code zu finden und zu beheben. Es ist auch sinnvoll, Fehlermeldungen in standard error zu schreiben, da dies dazu beiträgt, unseren Code sauber und leichter zu verstehen zu halten.

Einige Tipps beim Schreiben zu Standardfehlern:

- Stellen Sie sicher, dass der Fehlercode klar und präzise ist, damit Sie den Fehler leichter identifizieren können.
- Nutzen Sie aussagekräftige Fehlermeldungen, um zu verhindern, dass Sie stundenlang auf der Suche nach dem Grund für einen Fehler sind.
- Vergessen Sie nicht, die standard error Meldungen in Ihrer endgültigen Version Ihres Codes zu entfernen.

## Siehe auch

- [Kotlin Dokumentation](https://kotlinlang.org/docs/reference/exceptions.html)
- [Coding with Kotlin](https://www.codingwithkotlin.com/working-with-standard-error-in-kotlin/)