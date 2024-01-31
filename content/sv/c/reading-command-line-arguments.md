---
title:                "Läsa in kommandoradsargument"
date:                  2024-01-20T17:55:50.368354-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Kommandoradsargument låter användare interagera med ditt program genom att ge det instruktioner vid uppstart. Vi använder det för att göra program flexibla och användbara för flera olika scenarion.

## Hur gör man:

Här är ett exempel på hur man läser kommandoradsargument i C:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Programmet har fått %d argument:\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Om programmet körs såhär:

```
$ ./mittprogram hej värld
```

Kommer utskriften att vara:

```
Programmet har fått 2 argument:
Argument 1: hej
Argument 2: värld
```

## Djupdykning

Att läsa kommandoradsargument i C är standard sedan C-programmeringens barndom. Argumenten fångas i main-funktionens parametrar: `argc` (argument count) och `argv` (argument vector). `argc` talar om hur många argument som skickats till programmet och `argv` är en array av strängar (pekare till char) som representerar själva argumenten.

Det finns alternativ till att använda kommandoradsargument, såsom att läsa från filer, miljövariabler eller interaktiva prompts, men kommandoradsargument är ofta det snabbaste sättet att ge konfigurationer till ett program, särskilt för scripts och automatiseringar.

Implementeringsdetaljer inkluderar att `argv[0]` oftast innehåller programnamnet som körts och att argumenten som följer sedan börjar från `argv[1]`. Kom ihåg att alltid kontrollera `argc` innan du försöker använda ett argument för att undvika att läsa utanför arrayens gränser.

## Se även:

- C Standardbiblioteket dokumentation för argc och argv:
  https://en.cppreference.com/w/c/language/main_function
- GNU:s guide till GCC (GNU Compiler Collection) och dess hantering av kommandoradsargument:
  https://gcc.gnu.org/onlinedocs/gcc/
- För en genomgång av parsing-bibliotek för kommandoradsargument, se "argp.h" i GNU Standardbiblioteket:
  https://www.gnu.org/software/libc/manual/html_node/Argp.html
