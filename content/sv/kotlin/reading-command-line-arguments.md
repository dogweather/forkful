---
title:                "Kotlin: Läsning av kommandoradsargument"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

#Varför
Det finns många olika användningsområden för kommandoradsargument i Kotlin, till exempel att ge programmet användarinput eller för att styra vilken del av programmet som ska köras. Läs vidare för att lära dig hur du kan utnyttja kommandoradsargument i dina Kotlin-program.

#Så här gör du
För att läsa in kommandoradsargument i Kotlin behöver du först importera paketet "args" som ger dig tillgång till kommandoradsargumenten. Sedan kan du använda en for-loop för att iterera genom argumenten och utföra önskade åtgärder. Här är ett exempel på hur du kan skriva ut alla argumenten på skärmen:

```Kotlin
import kotlin.system.exitProcess // används för att avsluta programmet

fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
    exitProcess(0)
}
```

Om användaren till exempel har kört programmet med kommandoradsargumenten "Hello" och "World", kommer följande att skrivas ut:

```
Hello
World
```

#Djupdykning
Utöver att läsa in kommandoradsargument som strängar, kan du även omvandla dem till andra datatyper om det behövs. Till exempel kan du använda funktionen .toInt() för att omvandla en kommandoradssträng till en heltalsvariabel. Det finns även andra användbara funktioner och metoder du kan utforska för att anpassa användarinput med hjälp av kommandoradsargument.

#Se även
- [Kotlin documentation on command line arguments](https://kotlinlang.org/docs/command-line.html)
- [Tutorial on how to use command line arguments in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_command_line.htm)
- [Video tutorial on command line arguments in Kotlin](https://www.youtube.com/watch?v=QE3LNU029n8)