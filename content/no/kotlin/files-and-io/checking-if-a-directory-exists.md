---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:44.011820-07:00
description: "Hvordan: Kotlin, som kj\xF8rer p\xE5 JVM, benytter Java File API for\
  \ filoperasjoner, noe som gj\xF8r kontroll av mappetilstedev\xE6relse enkelt. Her\
  \ er et\u2026"
lastmod: '2024-03-13T22:44:40.765485-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, som kj\xF8rer p\xE5 JVM, benytter Java File API for filoperasjoner,\
  \ noe som gj\xF8r kontroll av mappetilstedev\xE6relse enkelt."
title: Sjekker om en mappe eksisterer
weight: 20
---

## Hvordan:
Kotlin, som kjører på JVM, benytter Java File API for filoperasjoner, noe som gjør kontroll av mappetilstedeværelse enkelt. Her er et grunnleggende eksempel:

```kotlin
import java.io.File

fun main() {
    val path = "/path/to/directory"
    val directory = File(path)

    if (directory.exists() && directory.isDirectory) {
        println("Directory exists: $path")
    } else {
        println("Directory does not exist: $path")
    }
}
```
Eksempel på utdata, med antagelse om at mappen eksisterer:
```
Directory exists: /path/to/directory
```
Og hvis den ikke gjør det:
```
Directory does not exist: /path/to/directory
```

I et Kotlin-prosjekt jobber du kanskje også ofte med Kotlin-spesifikke biblioteker eller rammeverk, som Ktor for webapplikasjoner eller kotlinx.coroutines for asynkron programmering. Men, for å sjekke om en mappe eksisterer, er den standard Java `File` API som vist typisk tilstrekkelig og mye brukt på grunn av Kotlins samarbeidsevne med Java. Det er ikke nødvendig med tredjepartsbiblioteker for denne spesifikke oppgaven, noe som gjør den tilgjengelig og grei for nybegynnere som går over fra andre programmeringsspråk til Kotlin.
