---
title:                "Überprüfen, ob ein Verzeichnis vorhanden ist."
html_title:           "Clojure: Überprüfen, ob ein Verzeichnis vorhanden ist."
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Wenn du in deiner Clojure-Anwendung auf Dateien zugreifen möchtest, kann es nützlich sein zu wissen, ob ein bestimmtes Verzeichnis existiert, bevor du versuchst, darauf zuzugreifen. Auf diese Weise kannst du sicherstellen, dass deine Anwendung nicht abstürzt, wenn ein erwartetes Verzeichnis nicht vorhanden ist.

## So geht's

Die Überprüfung, ob ein Verzeichnis in Clojure existiert, kann auf verschiedene Arten erfolgen. Eine Möglichkeit ist die Verwendung der `clojure.java.io/file` Funktion, die eine `java.io.File`-Instanz zurückgibt, die das angegebene Verzeichnis darstellt. Hier ist ein Beispiel, wie du diese Funktion verwenden kannst:

```Clojure
(require '[clojure.java.io :as io])

;; Erstelle eine `File`-Instanz für das Verzeichnis "test"
(def directory (io/file "test"))

;; Überprüfe, ob das Verzeichnis existiert
(.exists directory)
;; Output: true
```

Wenn das Verzeichnis nicht vorhanden ist, wird `false` zurückgegeben.

Du kannst auch die `clojure.java.io/file?` Funktion verwenden, um direkt zu überprüfen, ob ein Verzeichnis existiert. Diese Funktion gibt `true` zurück, wenn das angegebene Verzeichnis vorhanden ist, ansonsten `false`.

```Clojure
(require '[clojure.java.io :as io])

;; Überprüfe, ob das Verzeichnis "test" existiert
(io/file? "test")
;; Output: true

;; Überprüfe, ob das Verzeichnis "non-existent" existiert
(io/file? "non-existent")
;; Output: false
```

## Tiefergehende Informationen

Die Verwendung der `clojure.java.io/file` oder `clojure.java.io/file?` Funktion ist eine einfache Möglichkeit, um zu überprüfen, ob ein Verzeichnis vorhanden ist. Es ist jedoch wichtig zu beachten, dass diese Funktionen nur prüfen, ob auf Dateien auf dem Dateisystem zugegriffen werden kann. Sie überprüfen NICHT, ob das Verzeichnis tatsächlich existiert oder ob du Berechtigungen hast, auf das Verzeichnis zuzugreifen.

Eine alternative Möglichkeit wäre die Verwendung der Java-`java.nio.file.Files.Exists` Methode, die verwendet werden kann, um sowohl das Vorhandensein als auch die Berechtigungen für einen Verzeichnispfad zu überprüfen.

Zusammenfassend ist das Überprüfen, ob ein Verzeichnis in Clojure existiert, eine nützliche Technik, um sicherzustellen, dass deine Anwendung robust und fehlertolerant ist. Es ist wichtig, die verschiedenen Möglichkeiten zu verstehen und die beste Methode für deine spezifischen Anforderungen auszuwählen.

## Siehe auch

- Offizielle Clojure-Dokumentation für `clojure.java.io/file` und `clojure.java.io/file?`: https://clojure.github.io/clojure/clojure.java.io-api.html
- Java-`java.nio.file.Files.Exists` Methode: https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html#exists(java.nio.file.Path,%20java.nio.file.LinkOption...)