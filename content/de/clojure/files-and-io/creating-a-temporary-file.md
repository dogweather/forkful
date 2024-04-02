---
date: 2024-01-20 17:40:03.175050-07:00
description: "Tempor\xE4re Dateien dienen dazu, Daten zwischenzuspeichern, die w\xE4\
  hrend der Laufzeit eines Programms ben\xF6tigt, aber nicht dauerhaft gespeichert\
  \ werden\u2026"
lastmod: '2024-03-13T22:44:53.439558-06:00'
model: gpt-4-1106-preview
summary: "Tempor\xE4re Dateien dienen dazu, Daten zwischenzuspeichern, die w\xE4hrend\
  \ der Laufzeit eines Programms ben\xF6tigt, aber nicht dauerhaft gespeichert werden\u2026"
title: "Erstellung einer tempor\xE4ren Datei"
weight: 21
---

## Was & Warum?
Temporäre Dateien dienen dazu, Daten zwischenzuspeichern, die während der Laufzeit eines Programms benötigt, aber nicht dauerhaft gespeichert werden sollen. Sie sind hilfreich, um Speicherplatz zu schonen und die Sicherheit zu erhöhen, da sie nach Gebrauch gelöscht werden können.

## How to:
Erzeugen einer temporären Datei in Clojure:

```Clojure
(require '[clojure.java.io :as io])

(let [temp-file (io/file (io/create-temp-file "mein-prefix" ".txt"))]
  (spit temp-file "Das ist nur ein Test.")
  (println "Temporäre Datei erstellt unter:" (.getPath temp-file))
  ;; Verwende die Datei ...
  ;; Lösche die temporäre Datei, wenn sie nicht mehr benötigt wird
  (.delete temp-file))
```

Ausgabe:

```
Temporäre Datei erstellt unter: /tmp/mein-prefix1234567890.txt
```

## Deep Dive:
Temporäre Dateien sind nicht neu. Sie wurden schon in frühen Betriebssystemen verwendet, um mit begrenztem Speicher zu arbeiten. Clojure, auf der JVM basierend, nutzt Java's `File` API, um temporäre Dateien zu erstellen und zu handhaben. Eine Alternative ist die Benutzung von Memory-Mapped Files für große Datenmengen. Ein Memory-Mapped File ordnet Dateiinhalte direkt dem Speicher zu, was schneller sein kann, da es Dateisystemoperationen reduziert.

Hinsichtlich Implementierung entscheidet das Betriebssystem, wo die Datei gespeichert wird, meist in einem speziellen Temp-Verzeichnis. Je nach System können diese Dateien nach einem Neustart oder nach einer bestimmten Zeit gelöscht werden. Das manuelle Löschen im Programm sichert, dass keine Altlasten übrig bleiben.

## Siehe Auch:
- [Clojure Docs – clojure.java.io](https://clojuredocs.org/clojure.java.io)
- [Java Dokumentation – File.createTempFile](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)
- [Oracle Technical Network – Memory-Mapped Files in Java](https://www.oracle.com/technical-resources/articles/javase/nio.html)
