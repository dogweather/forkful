---
title:                "Nutzung einer interaktiven Shell (REPL)"
date:                  2024-01-26T04:15:33.148867-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nutzung einer interaktiven Shell (REPL)"

category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/using-an-interactive-shell-repl.md"
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
