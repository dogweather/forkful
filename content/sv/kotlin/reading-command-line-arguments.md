---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Kommandoradsargument är de data som du matar in direkt i kommandoradsapplikationen när du kör den. Programmerare använder detta för att göra sina program mer flexibla och interaktiva.

## Hur man gör:

Här är ett enkelt exempel på hur du läser kommandoradsargument i Kotlin:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

I det här exemplet kommer varje argument du matar in när programmet körs att skrivas ut i konsolen. Till exempel:

```Shell 
\$ kotlin MainKt.kt Hej världen 
Hej 
världen
```

## Fördjupning:

Historiskt sett har läsning av kommandoradsargument alltid varit en grundläggande funktion i många programmeringsspråk, med rötter i tidiga operativsystem som Unix.

Det finns också alternativ till standardmetoden att läsa kommandoradsargument, bland annat med användning av bibliotek som Apache Commons CLI och Argparser för att ge mer avancerade tolkningsegenskaper.

När det gäller implementeringsdetaljer, beror det på hur din Kotlin-kod körs. Om koden körs som en scriptfil (.kts), behandlas argumenten på samma sätt som i Java.

## Se även:

1. [Kotlin Dokumentation - Kommandoradsprogram](https://kotlinlang.org/docs/command-line.html)
2. [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
3. [Argparser](https://github.com/xenomachina/kotlin-argparser)