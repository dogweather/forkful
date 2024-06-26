---
date: 2024-01-20 18:03:20.162891-07:00
description: "How to: Leiningen ist das de-facto Standard-Tool, um Clojure-Projekte\
  \ zu verwalten. Es ist seit 2010 verf\xFCgbar und wurde von Phil Hagelberg entwickelt.\u2026"
lastmod: '2024-04-05T21:53:55.378100-06:00'
model: gpt-4-1106-preview
summary: Leiningen ist das de-facto Standard-Tool, um Clojure-Projekte zu verwalten.
title: Einen neuen Projekt starten
weight: 1
---

## How to:
```Clojure
;; Installiere Leiningen, das Build-Tool für Clojure:
;; https://leiningen.org/

;; Erstellen eines neuen Clojure-Projekts:
lein new app mein-tolles-projekt

;; Wechsle in das Projektverzeichnis:
cd mein-tolles-projekt

;; Starte eine interaktive Repl-Sitzung im Projekt:
lein repl

;; Der REPL startet, zeige die Projektstruktur:
ls
; Beispiel-Output:
; doc/  project.clj  README.md  resources/  src/  test/

;; Führe die Anwendung aus:
lein run
```

## Deep Dive
Leiningen ist das de-facto Standard-Tool, um Clojure-Projekte zu verwalten. Es ist seit 2010 verfügbar und wurde von Phil Hagelberg entwickelt. Alternativen sind Boot und neuerdings auch tools.deps - direkt von Clojure bereitgestellt. Leiningen hilft bei Dependency Management, Automatisierung von Tasks und Packaging. Es nutzt `project.clj` für die Konfiguration und unterstützt sowohl einfache Skripte als auch komplexe Anwendungen. Beim Start eines neuen Projekts erstellt es ein Grundgerüst, was den Einstieg erleichtert.

## See Also
- Leiningen Homepage: [https://leiningen.org/](https://leiningen.org/)
- Clojure Official Getting Started Guide: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
- Ein alternatives Build-Tool, Boot: [https://boot-clj.com/](https://boot-clj.com/)
- Clojure's neue Tools für die Dependency-Verwaltung: [https://clojure.org/reference/deps_and_cli](https://clojure.org/reference/deps_and_cli)
