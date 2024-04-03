---
date: 2024-01-26 04:17:49.451611-07:00
description: "Die Verwendung einer interaktiven Shell oder einer Read-Eval-Print-Schleife\
  \ (REPL) erm\xF6glicht es Ihnen, interaktiv zu programmieren. Programmierer nutzen\u2026"
lastmod: '2024-03-13T22:44:54.227188-06:00'
model: gpt-4-0125-preview
summary: "Die Verwendung einer interaktiven Shell oder einer Read-Eval-Print-Schleife\
  \ (REPL) erm\xF6glicht es Ihnen, interaktiv zu programmieren."
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

## Wie:
Rufen Sie REPL auf, indem Sie ein Terminal öffnen und `swift` ausführen. Geben Sie direkt Code ein und drücken Sie Enter, um ihn auszuführen. Hier ist ein Vorgeschmack:

```Swift
1> let greeting = "Hallo, REPL!"
greeting: String = "Hallo, REPL!"
2> print(greeting)
Hallo, REPL!
```

Beenden Sie mit `:quit` oder `Strg-D`.

## Tiefere Einblicke
Die Wurzeln von REPL reichen weit zurück zu den Lisp-Interpretern in den 60er Jahren. Swifts REPL basiert auf LLVM, einem leistungsstarken Compiler-Framework, und bietet mehr als nur eine einfache Interpretation – es ist ein vollwertiges Werkzeug mit Autovervollständigung, Debugging und mehr. REPL ist großartig zum Lernen oder Prototyping, aber es ist keine eigenständige Entwicklungsumgebung. Einige Leute bevorzugen für einen grafischeren, dateibasierten Ansatz die Verwendung von Playgrounds in Xcode, während andere bei traditionellem Skript-Editing und -Ausführen bleiben.

Unter der Haube kompiliert Swifts REPL den Code dynamisch in Maschinensprache und führt ihn aus, weshalb es relativ schnell ist. Es kann auch auf alle kompilierten Swift-Module oder sogar C-Bibliotheken zugreifen, was es ziemlich leistungsfähig macht. Beachten Sie jedoch, dass nicht alles perfekt in REPL funktioniert; einige Swift-Funktionen, insbesondere solche, die komplexe Projektsetups oder Storyboard-Dateien erfordern, funktionieren hier nicht.

## Siehe auch
- [Swift.org - Erste Schritte](https://www.swift.org/getting-started/#using-the-repl)
- Apples [Einführung in Xcode Playgrounds](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM-Projekt](https://llvm.org/)
