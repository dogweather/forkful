---
title:    "C: Läsa kommandoradargument"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att läsa och hantera kommandoradsargument är en viktig del av C-programmering. Det låter dig ta input från användaren när programmet körs och ger ett sätt att anpassa beteendet för olika situationer. Om du vill lära dig mer om hur man gör det, fortsätt läsa!

## Hur man gör det

För att läsa kommandoradsargument kan du använda funktionen `main ()` tillsammans med två parametrar: `argc` och `argv`. `argc` (argument count) räknar antalet argument som skickas med när programmet körs och `argv` (argument vector) är en array som innehåller själva argumenten. 

Här är ett exempel på kod som läser in ett kommandoradsargument och skriver ut det:

```C
#include <stdio.h>

int main(int argc, char** argv) {
  if (argc > 1) {
    printf("Det första kommandoradsargumentet är: %s\n", argv[1]);
  }
  return 0;
}
```

Om du skulle köra detta program med kommandoradsargumentet "hej", skulle följande skrivas ut: "Det första kommandoradsargumentet är: hej".

Du kan också använda en `for`-loop för att iterera genom alla argument som har skickats med:

```C
#include <stdio.h>

int main(int argc, char** argv) {
  for (int i = 1; i < argc; i++) {
    printf("Argument #%d är: %s\n", i, argv[i]);
  }
  return 0;
}
```

Om du skulle köra detta program med argumenten "hej" och "världen", skulle följande skrivas ut:

"Argument #1 är: hej"

"Argument #2 är: världen"


## Djupdykning

Nu när du har lärt dig att läsa och hantera kommandoradsargument, kan du gå djupare och utforska fler möjligheter. Till exempel kan du använda funktionen `getopt()` för att hantera olika flaggor och argumentvärden. Du kan också lära dig att hantera felaktiga argument och skriva ut hjälptexter för användaren.

Det finns också olika bibliotek tillgängliga som kan hjälpa dig att hantera kommandoradsargument, till exempel [argtable](https://www.argtable.org/) och [getopt_long](https://gnu.org/software/libc/manual/html_node/Getopt-Long-Option-Example.html).

## Se även

* [Programmera i C för nybörjare](https://www.learnc.org/beginners/c)
* [C-programmering: en inledning](https://www.tutorialspoint.com/cprogramming/index.htm)
* [C-kurs på Codeacademy](https://www.codecademy.com/courses/learn-c/)

Lycka till med att använda kommandoradsargument i dina C-program!