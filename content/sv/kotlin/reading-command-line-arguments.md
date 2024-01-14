---
title:                "Kotlin: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till att läsa kommandoradsargument när man programmerar i Kotlin. Genom att följa dessa tips kan du utöka funktionaliteten hos dina program och skapa mer flexibla och anpassningsbara lösningar.

## Så här

Att läsa kommandoradsargument i Kotlin är relativt enkelt. Först behöver du importera "args" från Kotlin.system.biblioteket. Sedan kan du använda "args" variabeln för att läsa in argumenten och utföra önskade operationer. Nedan är ett exempel på hur man läser in kommandoradsargument och skriver ut dem:

```Kotlin
import kotlin.system.*

fun main(args: Array<String>) {
    println("Det här programmet tar emot följande argument:")
    for(arg in args) {
        println(arg)
    }
}
```

Om du kör detta program i en kommandofönster och till exempel matar in "Kotlin är fantastiskt", kommer du att få utskriften:

```
Det här programmet tar emot följande argument:
Kotlin
är
fantastiskt
```

Som du kan se är det väldigt enkelt att läsa in kommandoradsargument och använda dem i ditt program. Du kan också använda olika metoder och funktioner för att behandla argumenten på olika sätt, beroende på vad du behöver.

## Djupdykning

När du läser in kommandoradsargument, är det viktigt att förstå hur argumenten ska skrivas och hur de tas emot av ditt program. Argumenten bör skrivas i form av en sträng, med varje argument åtskilt av ett mellanslag. Det är också möjligt att skicka in flera argument genom att separera dem med mellanslag.

Det är också värt att nämna att kommunikationen mellan ditt program och kommandofönstret är asynkron, vilket betyder att ditt program måste hålla reda på eventuella fördröjningar i inmatningen.

För att undvika problem med felaktiga argument som skriver över varandra, bör du alltid använda "trim()" metoden för trimning av argumentsträngar.

## Se även

Här är några användbara länkar där du kan läsa mer om hur man läser och hanterar kommandoradsargument i Kotlin:

- [Dokumentation för argv i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-argv/index.html)
- [3 enkla sätt att läsa in kommandoradsargument i Kotlin](https://www.sitereq.com/post/kotlin-read-command-line-arguments)
- [Kommandoradsargument och flaggor i Kotlin](https://kotlinexpertise.com/command-line-arguments-kotlin/)