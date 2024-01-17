---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Clojure: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist eine gängige Aufgabe für Programmierer. Dies ermöglicht ihnen zu prüfen, ob ein bestimmter Ordner auf einem System vorhanden ist, bevor sie versuchen, auf ihn zuzugreifen.

# Wie geht's:
```Clojure
;; Um zu überprüfen, ob ein Verzeichnis existiert, können wir die Clojure-Funktion (clojure.java.io/file "Pfad/zum/Verzeichnis") verwenden. Diese Funktion gibt ein File-Objekt zurück, das als "wahr" ausgewertet wird, wenn das Verzeichnis existiert, und als "falsch" wenn nicht.
(clojure.java.io/file "/Users/Benutzername/Downloads")
;; => #object[java.io.File "Pfad/zum/Verzeichnis"]

;; Wir können auch die Funktion "exists?" verwenden, um zu überprüfen, ob ein Verzeichnis existiert.
(.exists (clojure.java.io/file "/Users/Benutzername/Downloads"))
;; => true
```

# Tiefere Einblicke:
Das Überprüfen der Existenz eines Verzeichnisses ist Teil der "Datei- und Verzeichnisoperationen" in Clojure. Vor der Einführung von Clojure 1.7 mussten Programmierer jedoch die Java-Klasse "java.io.File" verwenden, um Verzeichnisse zu überprüfen. Es ist auch wichtig zu beachten, dass die Funktion "exists?" auch auf Dateien angewendet werden kann.

# Siehe auch:
- https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/reset-bang
- https://clojure.org/reference/java_interop#_exist_7za1&page=Clojure