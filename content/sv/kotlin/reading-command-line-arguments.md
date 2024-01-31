---
title:                "Läsa in kommandoradsargument"
date:                  2024-01-20T17:56:16.894419-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Kommandoradsargument låter program veta vad användaren vill direkt vid start. De används för att skräddarsy programkörningen utan en massa frågor efter att programmet startats.

## How to:
För att läsa in argument från kommandoraden i Kotlin, använd `args` som finns tillgängliga i funktionens `main`:

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hej, ${args[0]}!")
    } else {
        println("Hej, okänd användare!")
    }
}
```

Kör programmet så här:

```
$ kotlin MyProgram.kt Kalle
Hej, Kalle!
```

Om inga argument ges:

```
$ kotlin MyProgram.kt
Hej, okänd användare!
```

## Deep Dive
Innan Java 5.0 (Kotlin bygger på JVM), användes en alternativ metod för kommandoradsargument och JavaBeans var en favorit för att hantera konfiguration. I Kotlin är `args` en vanlig `Array` och man hanterar den på samma sätt som andra arrayer.

Det finns flera sätt att hantera mer avancerade behov:
- `kotlinx-cli` biblioteket erbjuder en deklarativ parser för komplexa uppsättningar av kommandoradsargument.
- Externa bibliotek som `picocli` eller `jcommander`.

Historiskt sett har flaggor och "switches" anpassat program uppsättningar, men Kotlin håller det clean och lätt med direkta argument till `main` funktionen.

## See Also
- Officiell Kotlin-dokumentation av kommandoradsargument: [kotlinlang.org](https://kotlinlang.org/docs/command-line.html)
- kotlinx-cli GitHub repo: [github.com/Kotlin/kotlinx-cli](https://github.com/Kotlin/kotlinx-cli)
- Picocli: [picocli.info](https://picocli.info)
- JCommander: [jcommander.org](http://jcommander.org)
