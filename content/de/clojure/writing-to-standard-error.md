---
title:                "Clojure: Schreiben an die Standardfehlerausgabe"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben in den Standardfehler ist ein nützliches Werkzeug für Entwickler und Programmierer, um Fehlermeldungen und Debugging-Informationen in ihre Programme einzubetten.

## Wie Es Geht

Die Verwendung des `clojure.core`-Moduls in Kombination mit dem `clojure.java.io`-Modul ermöglicht es uns, problemlos in den Standardfehler zu schreiben. Sehen wir uns ein Beispiel an:

```Clojure
(ns standard-error-demo.core
  (:require [clojure.java.io :as io]))

(defn write-to-stderr [msg]
  (binding [*err* (io/writer System/err)]
    (print msg)))

(write-to-stderr "Eine Fehlermeldung")
```

Output:

```
Eine Fehlermeldung
```

## Tiefer Einblick

Das `binding`-Makro bindet zeitweise einen neuen Wert an eine Variable und stellt sicher, dass der ursprüngliche Wert nach einer Funktion oder einem Block wiederhergestellt wird. Durch die Bindung der `*err*`-Variable an `System/err`, können wir den Standardfehler so lange ändern, wie wir uns innerhalb der `write-to-stderr`-Funktion befinden.

Zusätzlich bietet Clojure verschiedene andere Möglichkeiten, um in den Standardfehler zu schreiben, wie zum Beispiel die Verwendung des `println`-Befehls mit der `System/err`-Variable oder die Verwendung des `eprintln`-Befehls mit dem `clojure.java.shell`-Modul.

## Siehe Auch

- [Clojure Dokumentation zu `binding`](https://clojuredocs.org/clojure.core/binding)
- [Clojure Dokumentation zu `println` und `eprintln`](https://clojuredocs.org/clojure.core/println)
- [Clojure Dokumentation zu `clojure.java.shell`](https://clojuredocs.org/clojure.java.shell)