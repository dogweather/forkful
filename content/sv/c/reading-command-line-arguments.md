---
title:                "C: Läsning av kommandoradsargument"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa kommandoradsargument är en viktig del av att behärska C-programmering. Genom att kunna läsa in parametrar via kommandoraden kan du göra dina program mer flexibla och anpassningsbara. Detta kan vara särskilt användbart om du vill skapa program som kan ta emot olika typer av input från användaren. Men varför ska man lägga tid och ansträngning på att lära sig detta? Låt oss ta en titt på fördelarna med att kunna läsa kommandoradsargument.

## Hur man gör

Att läsa kommandoradsargument i C är relativt enkelt. Först och främst behöver du inkludera <stdio.h> för att kunna använda funktionen "main" som är där vi kommer att läsa in argumenten. Sedan kan du läsa in argumenten genom att använda "argc" och "argv" variablerna. "argc" håller antalet argument som matats in och "argv" är en array som innehåller själva argumenten. Låt oss titta på ett exempel:

```C
#include <stdio.h>

int main(int argc, char* argv[]) {
    for(int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

I detta enkla program loopar vi igenom alla argument och skriver ut dem med hjälp av "printf" funktionen. Om vi kör detta program med kommandot "program --option1 arg1 arg2" så bör vi få utskriften:

```
Argument 0: program
Argument 1: --option1
Argument 2: arg1
Argument 3: arg2
```

Som du ser så är "program" själva programmet som utförs, medan "-option1", "arg1" och "arg2" är argumenten som matats in.

## Djupdykning

Nu när vi förstår grunderna av att läsa kommandoradsargument, låt oss titta på några mer avancerade användningsområden. En vanlig användning är att ge programmet olika inställningar eller alternativ baserat på argumenten. Till exempel kan du ha ett alternativ "-h" som visar en hjälpmeddelande eller "-v" som står för "verbose" och visar mer detaljerad information.

En annan användning är för att hantera felaktiga argument. Om användaren matar in felaktiga argument till ditt program, kan du använda "argc" och "argv" för att ge en lämplig felhantering. Till exempel kan du skriva ut ett felmeddelande och avsluta programmet om användaren matar in för få argument.

Det finns också möjlighet att lägga till värden för olika argument. Till exempel kan du ha ett alternativ "--input" som kräver att användaren även matar in ett värde efter det, till exempel "--input 5". Detta kan vara särskilt användbart för att göra ditt program mer anpassningsbart.

## Se även

- [How to Parse Command Line Arguments in C](https://www.codeproject.com/Articles/7049/Quick-and-Dirty-Command-Line-argument-parse)
- [Command Line Arguments in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)