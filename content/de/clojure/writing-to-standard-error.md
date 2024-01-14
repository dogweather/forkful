---
title:    "Clojure: Schreiben auf Standardfehler"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Warum

Standard Error ist ein wichtiges Konzept in der Welt der Programmierung. Oftmals wird es verwendet, um Fehlermeldungen oder Debugging-Informationen auszugeben. In diesem Blog-Post werden wir uns genauer mit der Verwendung von Standard Error in Clojure auseinandersetzen und sehen, warum es eine nützliche Funktion ist.

# Wie es geht

Um eine Nachricht an den Standard Error Stream zu senden, können wir die Funktion `eprint` verwenden. Hier ist ein einfaches Beispiel:

```Clojure
(eprint "Dies ist eine Fehlermeldung")
```

Dieser Codeblock wird zu folgendem Output führen:

```
Dies ist eine Fehlermeldung
```

Standard Error ist besonders nützlich, wenn wir während der Ausführung einer Funktion oder eines Programms Fehlermeldungen ausgeben möchten. Wir können das mit einer `try`/`catch`-Block-Struktur erreichen, wie im folgenden Beispiel:

```Clojure
(try
  (/ 5 0)
  (catch Exception e
    (eprint "Es gab einen Fehler: " (.getMessage e))))
```

Dieser Codeblock wird zu folgendem Output führen:

```
Es gab einen Fehler: Divison durch Null
```

# Tiefgründig

In der Standardbibliothek von Clojure gibt es mehrere Funktionen, die es uns ermöglichen, mit dem Standard Error Stream zu arbeiten. Ein Beispiel hierfür ist `set!`, mit dem wir den Stream umleiten können, um beispielsweise Debugging-Informationen in eine Datei zu schreiben. Hier ist ein Beispiel:

```Clojure
(def error-file (java.io.BufferedWriter. (java.io.FileWriter. "debug.log")))
(set! *err* error-file)
(eprint "Debugging-Informationen")
(.close error-file)
```

Dieser Codeblock wird die Nachricht "Debugging-Informationen" in die Datei "debug.log" schreiben.

Ein weiteres nützliches Feature von Standard Error ist die Möglichkeit, Farbcodes in die Ausgabe zu integrieren, um wichtige Informationen hervorzuheben. Ein Beispiel für die Verwendung von Farbcodes im Standard Error Stream finden Sie in der Dokumentation von Clojure (`(doc *err*)`).

# Siehe auch

- [Clojure Dokumentation zu *err*](https://clojuredocs.org/clojure.core/*err*)
- [Offizielle Clojure-Website](https://clojure.org/)
- [Clojure-Forum auf Reddit](https://www.reddit.com/r/clojure/)