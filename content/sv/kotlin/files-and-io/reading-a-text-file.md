---
aliases:
- /sv/kotlin/reading-a-text-file/
date: 2024-01-20 17:54:39.104745-07:00
description: "Att l\xE4sa en textfil inneb\xE4r att din Kotlin-kod h\xE4mtar data\
  \ fr\xE5n en fil som sparats p\xE5 disken. Programmerare g\xF6r detta f\xF6r att\
  \ hantera indata,\u2026"
lastmod: 2024-02-18 23:08:51.769117
model: gpt-4-1106-preview
summary: "Att l\xE4sa en textfil inneb\xE4r att din Kotlin-kod h\xE4mtar data fr\xE5\
  n en fil som sparats p\xE5 disken. Programmerare g\xF6r detta f\xF6r att hantera\
  \ indata,\u2026"
title: "L\xE4sa en textfil"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil innebär att din Kotlin-kod hämtar data från en fil som sparats på disken. Programmerare gör detta för att hantera indata, konfigurationer, eller för att processa stora mängder av information utan att behöva hårdkoda den i applikationen.

## Hur man gör:
```kotlin
import java.io.File

fun main() {
    val path = "example.txt"
    val readText = File(path).readText(Charsets.UTF_8)
    println(readText)
}
```
Exempelutskrift:
```
Detta är innehållet i din textfil.
```
Med `File(path).readText(Charsets.UTF_8)` läser du enkelt hela filen som en sträng.

För större filer som inte bör läsas in i minnet på en gång:
```kotlin
fun main() {
    val path = "big_example.txt"
    File(path).forEachLine { line ->
        println(line)
    }
}
```

## Djupdykning
Historiskt sett har filinläsning varit kärnan i många program och skript. I Java, som Kotlin ofta jämförs med, finns det många olika sätt att göra det på – `FileReader`, `BufferedReader`, `Scanner`, för att nämna några. Kotlin förenklar processen genom att erbjuda inbyggda funktioner som `readText` och `forEachLine`, vilket minskar boilerplate-koden. När det kommer till prestanda och minneshantering bör `forEachLine` användas för att processa filen rad för rad, vilket är effektivare för stora filer.

Alternativ för att läsa filer innehåller bland annat att använda `InputStream`, `readBytes` för binära filer, eller `bufferedReader()` för mer kontroll och effektivitet under inläsningen.

En annan viktig del är felhantering. Du bör alltid vara redo att hantera IOExceptions när du arbetar med filinläsning.

## Se även
- Oracle guide om fil I/O (ger en bra grund för att förstå grunderna i Java, som Kotlin bygger vidare på): [https://docs.oracle.com/javase/tutorial/essential/io/](https://docs.oracle.com/javase/tutorial/essential/io/)
- Stack Overflow diskussioner om att läsa filer i Kotlin för mer avancerade och specifika användningsfall: [https://stackoverflow.com/questions/tagged/kotlin+file-io](https://stackoverflow.com/questions/tagged/kotlin+file-io)
