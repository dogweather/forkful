---
aliases:
- /de/kotlin/using-an-interactive-shell-repl/
date: 2024-01-26 04:15:33.148867-07:00
description: "Ein REPL (Read-Eval-Print Loop) ist eine einfache, interaktive Programmierumgebung.\
  \ Programmierer nutzen es f\xFCr schnelle Codierungsversuche, das Testen\u2026"
lastmod: 2024-02-18 23:09:04.828169
model: gpt-4-0125-preview
summary: "Ein REPL (Read-Eval-Print Loop) ist eine einfache, interaktive Programmierumgebung.\
  \ Programmierer nutzen es f\xFCr schnelle Codierungsversuche, das Testen\u2026"
title: Nutzung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?
Ein REPL (Read-Eval-Print Loop) ist eine einfache, interaktive Programmierumgebung. Programmierer nutzen es für schnelle Codierungsversuche, das Testen von Code-Snippets oder das Erlernen der Syntax einer Sprache, ohne eine vollständige Anwendung erstellen zu müssen.

## Wie:
Das Starten des Kotlin-REPL ist ein Kinderspiel. Öffne dein Terminal und tippe `kotlinc` ein. Du wirst in die Kotlin-Shell gelangen. Lass uns versuchen, eine Variable zu definieren und ihren Wert auszugeben:

```kotlin
Willkommen bei Kotlin Version 1.7.10 (JRE 1.8.0_292-b10)
Tippe :help für Hilfe, :quit zum Beenden
>>> val begruessung = "Hallo, Kotlin REPL!"
>>> println(begruessung)
Hallo, Kotlin REPL!
```

## Tiefergehend
Das REPL von Kotlin wurde zusammen mit der Sprache eingeführt, um Experimentieren zu fördern. Es ähnelt der interaktiven Shell von Python, ist jedoch auf die Syntax und Eigenheiten von Kotlin zugeschnitten. Alternativen? Interaktive Umgebungen in IDEs wie IntelliJ IDEA und Online-Kotlin-Playgrounds. Das REPL funktioniert, indem es Code sofort kompiliert und damit sofortiges Feedback liefert – entscheidend für das Lernen und Debuggen.

## Siehe auch
- Kotlin-Dokumentation über REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Kotlin im Browser ausprobieren: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- JetBrains Kotlin Playground-Plugin für IntelliJ IDEA.
