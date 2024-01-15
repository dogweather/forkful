---
title:                "Läsning av kommandoradsargument"
html_title:           "Java: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Om du är ny till programmering, kan det kännas överväldigande att lära sig nya sätt att interagera med din kod. Men att lära sig att läsa kommandoradsargument kan vara användbart för att göra dina program mer anpassningsbara. Det ger också möjlighet att interagera med användare genom att läsa inmatade värden direkt från kommandoraden istället för att visa en prompt.

## Hur man gör det
För att läsa kommandoradsargument i Java, behöver du använda "args" parametern i main-metoden. 
Här är ett exempel på hur man skriver ut ett kommandoradsargument i terminalen:

```Java
public static void main(String[] args) {
    System.out.println(args[0]);
}

// Om du kör programmet med "java MinProgram argument1" kommer det att skriva ut "argument1" i terminalen.
```

Kommandoradsargument lagras som en array av strängar, där varje argument är en egen sträng i arrayen. Du kan använda en for-loop för att gå igenom alla argument och utföra olika operationer beroende på vad som matas in.

```Java
public static void main(String[] args) {
    for (int i = 0; i < args.length; i++) {
        System.out.println("Argument " + i + ": " + args[i]);
    }
}

// Om du kör programmet med "java MinProgram argument1 argument2 argument3" kommer det att skriva ut:
// Argument 0: argument1
// Argument 1: argument2
// Argument 2: argument3
```

## Djupdykning
En av fördelarna med att läsa kommandoradsargument är att du kan ange olika parametrar för ditt program som hjälper till att anpassa det för olika användare. Du kan också använda flaggor för att aktivera eller inaktivera olika funktioner i ditt program beroende på vad användaren behöver.

```Java
public static void main(String[] args) {
    if(args[0].equals("-s")) {
        // Om användaren matar in flaggan "-s" utför programmet en specifik funktion.
    } else if (args[0].equals("-v")) {
        // Om användaren matar in flaggan "-v" utför programmet en annan funktion.
    } else {
        // Om ingen flagga anges, utför standardfunktionen för programmet.
    }
}
```

Det är viktigt att notera att när du läser kommandoradsargument måste du hantera felaktiga inmatningar eller brist på input. Annars kan ditt program krascha eller inte fungera som förväntat. Det är också bra att ha en hjälp- eller användarmanual som förklarar vilka flaggor och argument som kan matas in för ditt program.

## Se även
- [Java documentation on Command Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [GeeksforGeeks article on Command Line Arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)