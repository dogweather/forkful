---
title:    "Clojure: Schreiben auf den Standardfehler"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Standardfehlern (standard error) ist eine nützliche Technik beim Programmieren in Clojure. Es ermöglicht es Entwicklern, Fehler und Warnungen in ihren Programmen zu erkennen und zu beheben.

## Wie man es macht

Um etwas in die standard error-Konsole zu schreiben, können wir die `System/err` Funktion verwenden. Zum Beispiel:

```Clojure
(System/err "Dies ist eine Fehlermeldung!")
```

Die Ausgabe sieht folgendermaßen aus:

```
Dies ist eine Fehlermeldung!
```

Dies ist besonders hilfreich, wenn wir versuchen, Fehler in unserem Code zu finden und zu beheben, da sie in der Konsole sichtbar werden und uns dabei unterstützen. Wir können auch die `println` Funktion verwenden, um zusätzliche Informationen zu unserem Fehler auszugeben.

```Clojure
(println "Fehler beim Ausführen des Codes!")
(System/err "Dies ist eine Fehlermeldung!")
```

Die Ausgabe sieht jetzt so aus:

```
Fehler beim Ausführen des Codes!
Dies ist eine Fehlermeldung!
```

## Tiefergehende Informationen

In Clojure gibt es verschiedene Mittel, um mit Fehlern und Warnungen umzugehen. Eine davon ist das Verwenden von `clojure.main/repl` Funktion, um in einer interaktiven Umgebung mit unserem Code zu experimentieren. Hier können wir auch die `pst` Funktion verwenden, um das Stack-Trace unseres Fehlers zu sehen und zu analysieren.

Eine weitere Möglichkeit ist das Verwenden von `try/catch` Blöcken, um mögliche Fehler in unserem Code abzufangen und damit umzugehen.

Es ist auch wichtig zu beachten, dass das Schreiben von Nachrichten in die standard error-Konsole nicht immer die beste Option ist, da sie möglicherweise nicht in bestimmten Situationen sichtbar sind. In solchen Fällen kann das Verwenden von Protokollierungsfunktionen wie `log/error` besser sein.

## Siehe auch

- Dokumentation für die `System/err` Funktion: [https://clojuredocs.org/clojure.core/System/err](https://clojuredocs.org/clojure.core/System/err)
- Dokumentation für `clojure.main/repl`: [https://clojuredocs.org/clojure.main-repl](https://clojuredocs.org/clojure.main-repl)
- Informationen über Ausnahmebehandlung in Clojure: [https://clojure.org/guides/exceptions](https://clojure.org/guides/exceptions)