---
title:    "Clojure: Überprüfen, ob ein Verzeichnis existiert."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis vorhanden ist, ist eine wichtige Aufgabe in der Programmierung. Es kann hilfreich sein, um sicherzustellen, dass bestimmte Dateien oder Ressourcen vorhanden sind, bevor sie verwendet werden, und um zu vermeiden, dass Fehler auftreten.

## Wie geht man vor

Die "file-seq" Funktion in Clojure ermöglicht es uns, alle Dateien und Verzeichnisse unter einem angegebenen Pfad aufzulisten. Um zu überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist, können wir diese Funktion verwenden und die Ausgabe mit dem gewünschten Verzeichnis vergleichen.

```Clojure
(def directory "/Users/Benutzername/Dokumente")

(def directory-exists? 
  (some #(= % directory) (map :path (file-seq "/Users/Benutzername/"))))

(println directory-exists?)
;; Output: true
```

## Tiefer Einblick

Beim Überprüfen, ob ein Verzeichnis vorhanden ist, ist es wichtig, den vollständigen Pfad zu angeben. Wenn wir nur den relativen Pfad verwenden, kann es sein, dass die Funktion nicht das gewünschte Ergebnis zurückgibt. Außerdem ist es ratsam, sicherzustellen, dass der angegebene Pfad ein gültiges Verzeichnis ist, bevor wir versuchen, darauf zuzugreifen.

## Siehe auch

- Die offizielle Dokumentation von Clojure zur "file-seq" Funktion: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/file-seq
- Ein Tutorial zur Arbeit mit Dateien und Verzeichnissen in Clojure: https://www.tutorialspoint.com/clojure/clojure_working_with_files.htm
- Eine Übersicht über die gängigsten Datei- und Verzeichnisoperationen in Clojure: https://www.baeldung.com/clojure-file-directory-operations