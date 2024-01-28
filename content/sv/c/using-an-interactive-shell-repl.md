---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:11:40.788142-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En interaktiv skal, eller Läsa-Evalueras-Skriva Ut-Loop (REPL), är ett verktyg som erbjuder en kodningsmiljö i realtid för att omedelbart testa kodsnuttar. Programmerare använder den för snabb återkoppling under utveckling, inlärning och felsökning.

## Hur:
C kommer inte med en inbyggd REPL, men du kan använda tredjepartsverktyg. Här är en inblick med Cling, en C++ tolk som också kan hantera C-kod:

```C
#include <stdio.h>

int main() {
    printf("Hej, REPL-världen!\n");
    return 0;
}
```

Utskrift i Cling REPL:
```
[cling]$ .x yourscript.c
Hej, REPL-världen!
```

Cling kör skriptet och skriver ut resultatet omedelbart.

## Fördjupning
REPLs är standard i dynamiska språk som Python eller Ruby, men för kompilerade språk som C är de mindre vanliga. Historiskt sett lånade sig inte kompilera-köra-felsöka-cykeln till interaktiv utforskning. Verktyg som Cling och online C-kompilatorer erbjuder REPL-liknande upplevelser genom att kapsla in din C-kod i en C++ miljö.

Alternativ till Cling inkluderar C-tolkar som CINT och Ch. Dessa verktyg möjliggör snabba iterationer men kanske inte är lämpliga för alla utvecklingsscenarion på grund av prestationsbegränsningar och stöd för komplexa funktioner.

Implementering av en REPL i ett kompilerat språk innebär att kompilera och köra kodsnuttar på flygande fot, vilket inte är trivialt och kan ha begränsningar jämfört med hela språkets kapaciteter.

## Se även
- Cling: https://github.com/root-project/cling
- Online C Compiler och REPL: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Ch Tolken: http://www.softintegration.com/products/chstandard/
