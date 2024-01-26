---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Arduino: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Prüfen, ob ein Verzeichnis existiert, bedeutet zu kontrollieren, ob ein bestimmter Pfad auf dem Dateisystem zu einem realen Ordner führt. Programmierer machen das, um Fehler zu vermeiden, die entstehen, wenn sie auf nicht vorhandene Verzeichnisse zugreifen wollen.

## So geht’s:
In Clojure verwenden wir die `java.io.File` Klasse und die `exists` Methode, um zu prüfen, ob ein Verzeichnis existiert. Hier ist ein einfaches Beispiel:

```clojure
(import '[java.io File])

(defn directory-exists? [path]
  (.exists (File. path)))

(println (directory-exists? "/mein/existierendes/verzeichnis")) ; => true
(println (directory-exists? "/nicht/existierendes/verzeichnis")) ; => false
```

## Tieftauchen:
Diese Funktionalität basiert auf Java, da Clojure auf der Java Virtual Machine (JVM) läuft. In früheren Zeiten, vor Dateisystem-Abstraktionen, war der Prozess OS-spezifisch und komplexer. Alternativen zur Verwendung von `java.io.File` sind die `java.nio.file.Files` Klasse und die `exists` Methode zusammen mit `Paths` (ab Java 7), die eine modernere API bietet:

```clojure
(import '[java.nio.file Files Paths])

(defn directory-exists-nio? [path]
  (Files/exists (Paths/get path (into-array String []))))

(println (directory-exists-nio? "/mein/existierendes/verzeichnis")) ; => true
(println (directory-exists-nio? "/nicht/existierendes/verzeichnis")) ; => false
```

Die `java.nio.file` API bietet zudem bessere Fehlerbehandlung und ist für größere Projekte generell empfehlenswert.

## Siehe auch:
- Clojure's offizielle Dokumentation: https://clojure.org/
- Java's `File` Dokumentation: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
- Java's `Path` und `Files` Dokumentation: https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html
- Ein praktischer Leitfaden zur Java NIO: https://www.baeldung.com/java-nio2-file-api
