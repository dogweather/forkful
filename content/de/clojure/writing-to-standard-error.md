---
title:                "Clojure: Schreiben zum Standardfehler"
simple_title:         "Schreiben zum Standardfehler"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum: Schreiben auf den Standardfehler

Das Schreiben auf den Standardfehler ist ein wichtiges Werkzeug für jeden Clojure-Programmierer. Es ermöglicht die Ausgabe von Fehlermeldungen und anderen wichtigen Informationen direkt in die Konsole, ohne dass das Programm unterbrochen werden muss.

# Wie geht man vor?

Um auf den Standardfehler zu schreiben, verwendet man einfach die Funktion "println" und gibt den gewünschten Text als Argument ein. Zum Beispiel:

```Clojure
(println "Dies ist eine Fehlermeldung")
```

Dieser Code wird den Text "Dies ist eine Fehlermeldung" in die Konsole ausgeben, damit der Entwickler die Information erhalten kann.

# Tiefentauchen

Beim Schreiben auf den Standardfehler ist es wichtig zu beachten, dass es unabhängig von der Standardausgabe läuft. Das bedeutet, dass die Reihenfolge der Ausgaben nicht garantiert ist und sie sich möglicherweise überschneiden können. Es ist auch wichtig, die richtigen Fehlermeldungen auszugeben, um dem Entwickler beim Debuggen des Codes zu helfen.

# Siehe auch

- [Official Clojure Documentation](https://clojure.org/)
- [Clojure Style Guide](https://guide.clojure.style/)