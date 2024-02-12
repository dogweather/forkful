---
title:                "Använda en interaktiv skal (REPL)"
aliases: - /sv/kotlin/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:15:46.863850-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En REPL (Read-Eval-Print Loop) är en enkel, interaktiv programmeringsmiljö. Programmerare använder den för snabba kodförsök, testa kodsnuttar, eller att lära sig ett språks syntax utan att skapa en fullständig applikation.

## Hur man gör:
Att starta Kotlin's REPL är enkelt. Öppna din terminal och skriv `kotlinc`. Du landar då i Kotlin-skalen. Låt oss försöka definiera en variabel och skriva ut dess värde:

```kotlin
Välkommen till Kotlin version 1.7.10 (JRE 1.8.0_292-b10)
Skriv :help för hjälp, :quit för att avsluta
>>> val hälsning = "Hej, Kotlin REPL!"
>>> println(hälsning)
Hej, Kotlin REPL!
```

## Fördjupning
Kotlins REPL debuterade med språket för att uppmuntra till experiment. Det liknar Pythons interaktiva skal men är anpassat för Kotlin's syntax och särdrag. Alternativ? Interaktiva miljöer i IDE:er, såsom IntelliJ IDEA, och online Kotlin-lekplatser. REPL fungerar genom att kompilera kod direkt, vilket ger omedelbar feedback – avgörande för lärande och felsökning.

## Se även
- Kotlin-dokumentation om REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Testa Kotlin i webbläsaren: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- JetBrains Kotlin Playground-plugin för IntelliJ IDEA.
