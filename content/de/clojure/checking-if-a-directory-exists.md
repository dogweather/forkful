---
title:    "Clojure: Überprüfen, ob ein Verzeichnis existiert."
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Schritt in der Entwicklung von Programmen, die mit Dateien und Ordnern arbeiten. Es stellt sicher, dass das Programm nicht abstürzt, wenn das erwartete Verzeichnis nicht vorhanden ist. Dies kann auch dazu beitragen, unerwünschte Fehler zu vermeiden und die Benutzerfreundlichkeit zu verbessern.

## Wie geht es

Um in Clojure zu überprüfen, ob ein Verzeichnis existiert, können Sie die Funktion `clojure.java.io/file` verwenden, die einen Dateiobjekt zurückgibt. Dann können Sie die Funktion `exists?` auf das Dateiobjekt anwenden und sehen, ob es `true` zurückgibt, was bedeutet, dass das Verzeichnis vorhanden ist, oder `false`, was bedeutet, dass es nicht vorhanden ist.

``` Clojure
(def directory (clojure.java.io/file "/pfad/zum/verzeichnis"))
(exists? directory) ; => true oder false, abhängig davon, ob das Verzeichnis existiert oder nicht 
```

Sie können auch die `try`- und `catch`-Blöcke verwenden, um mit Fehlern umzugehen, die auftreten können, wenn ein Dateipfad ungültig ist oder aus anderen Gründen. Beispiel:

```Clojure
(try
  (def directory (clojure.java.io/file "/ungültiger/pfad"))
  (exists? directory)
  (catch Exception e
    (println "Fehler beim Überprüfen des Verzeichnisses: " (.getMessage e))))
```

## Tiefentauchen

Die `file`-Funktion akzeptiert optional einen zweiten Parameter, `options`, der eine Sammlung von Kennzeichnern und deren Werten ist. Ein nützlicher Kennzeichner ist `:follow-links`, der angibt, ob Links beim Navigieren durch die Dateisystemstruktur gefolgt werden sollen oder nicht. Wenn er auf `true` gesetzt ist, können Sie sicherstellen, dass ein Verzeichnis auch dann als vorhanden angezeigt wird, wenn es sich tatsächlich um einen symbolischen Link handelt.

```Clojure
(def directory (clojure.java.io/file "/pfad/zum/verzeichnis" {:follow-links true}))
(exists? directory) ; => true, auch wenn das Verzeichnis ein symbolischer Link ist
```

Eine andere nützliche Funktion beim Umgang mit Dateien und Ordnern ist `file-seq`, die eine Sequenz von Dateien und Ordnern für einen bestimmten Pfad zurückgibt. Beispiel:

```Clojure
(def files (clojure.java.io/file-seq "/pfad/zum/verzeichnis"))
```

Dies gibt eine Sequenz von Dateiobjekten zurück, die Sie dann weiter verarbeiten oder mit der `exists?`-Funktion überprüfen können.

## Siehe auch
- [clojure.java.io/file-Dokumentation] (https://clojuredocs.org/clojure.java.io%2Ffile)
- [clojure.java.io-Dokumentation] (https://clojuredocs.org/clojure.java.io)
- [Clojure-Community auf Reddit] (https://www.reddit.com/r/Clojure/)