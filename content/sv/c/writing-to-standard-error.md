---
title:                "C: Skriva till standard fel"
simple_title:         "Skriva till standard fel"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Varför

Att skriva till standard error är ett viktigt koncept i C-programmering eftersom det ger möjlighet att skicka felmeddelanden och annan debugging-information direkt till användaren. Detta är särskilt användbart i större projekt där det finns flera delar av koden som behöver kommunicera med varandra.

# Hur man gör

För att skriva till standard error i C använder man funktionen `fprintf()` tillsammans med `stderr` som första argument. Här är ett enkelt exempel som skickar ett felmeddelande till standard error:

```C
fprintf(stderr, "Ett fel har inträffat\n");
```

När man använder `fprintf()` behöver man också specificera vilken typ av information man vill skicka, vilket görs genom att lägga till formatsträngar efter felmeddelandet. Till exempel, om man vill skriva ut värdet av en variabel `x` kan man göra så här:

```C
fprintf(stderr, "Värdet av x är: %d\n", x);
```

Man kan också använda `stderr` för att skriva ut andra typer av information, som till exempel varningar eller debugging-information. Det är också möjligt att kombinera `fprintf()` med `printf()` för att skriva till både standard output och standard error samtidigt.

# Djupdykning

I C-programmering finns det två standardströmmar som används för att kommunicera med användaren: standard input (`stdin`) och standard output (`stdout`). Standard input är där användaren skickar in information till programmet, medan standard output är där programmet skriver ut resultatet. Standard error (`stderr`) används för att skriva ut felmeddelanden och annan debugging-information.

Det är viktigt att skilja mellan `stdout` och `stderr` eftersom vissa program, som till exempel kommandoraden på en dator, skickar standard output till användarens skärm medan standard error skickas till en loggfil som användaren inte kan se. Detta betyder att det är lämpligt att använda `stderr` för viktig information som användaren behöver se direkt.

# Se även

- [Wikipedia - Standard streams](https://sv.wikipedia.org/wiki/Standardstr%C3%B6m)
- [Stack Overflow - Difference between printf and fprintf in C](https://stackoverflow.com/questions/9332816/difference-between-printf-and-fprintf-in-c)