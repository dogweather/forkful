---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:07.224157-07:00
description: "Debuggers in C zijn gespecialiseerde tools die ontwikkelaars in staat\
  \ stellen om stap voor stap door hun code te gaan, variabelen te inspecteren en\
  \ de\u2026"
lastmod: '2024-02-25T18:49:48.620544-07:00'
model: gpt-4-0125-preview
summary: "Debuggers in C zijn gespecialiseerde tools die ontwikkelaars in staat stellen\
  \ om stap voor stap door hun code te gaan, variabelen te inspecteren en de\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?

Debuggers in C zijn gespecialiseerde tools die ontwikkelaars in staat stellen om stap voor stap door hun code te gaan, variabelen te inspecteren en de uitvoeringsstroom te volgen. Dit proces is essentieel voor het identificeren en oplossen van bugs, om ervoor te zorgen dat de code zich gedraagt zoals verwacht.

## Hoe:

GDB (GNU Debugger) is de meest gebruikte debugger voor C-programmering. Hier is een beknopte handleiding voor het gebruik van GDB om een eenvoudig C-programma te debuggen.

Compileer eerst je C-programma met de `-g` vlag om debuginformatie op te nemen:

```c
gcc -g program.c -o program
```

Start vervolgens GDB met je gecompileerde programma:

```bash
gdb ./program
```

Je kunt nu verschillende commando's binnen GDB gebruiken om de werking te controleren. Hier zijn een paar fundamentele commando's:

- `break`: Zet een breekpunt op een gespecificeerde regel of functie om de uitvoering te pauzeren.
  - Voorbeeld: `break 10` of `break main`
- `run`: Start de uitvoering van je programma binnen GDB.
- `next`: Voer de volgende regel code uit zonder in functies te stappen.
- `step`: Voer de volgende regel code uit en stap in functies.
- `print`: Toon de waarde van een variabele.
- `continue`: Hervat de uitvoering tot het volgende breekpunt.
- `quit`: Verlaat GDB.

Hier is een voorbeeldsessie voor het debuggen van een eenvoudig programma:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Compileer en start GDB zoals beschreven. Zet een breekpunt op de `printf` regel met `break 5` en vervolgens `run`. Gebruik `next` om door de lus te stappen en `print i` om de lusvariabele te inspecteren.

Voorbeelduitvoer na het instellen van een breekpunt en voor de eerste iteratie:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

Gebruik `print i` na een paar iteraties:

```
$3 = 2
```

Dit demonstreert het onderzoeken van de staat en stroom van een eenvoudig programma.

## Diepgaande Duik

Het concept van debuggen is aanzienlijk geëvolueerd sinds de vroege dagen van programmeren, waar fysieke bugs (letterlijke insecten) problemen konden veroorzaken in mechanische computers. Tegenwoordig bieden debuggers zoals GDB geavanceerde functies die verder gaan dan basisstap- en variabele-inspectie, zoals reverse debugging (het programma achterwaarts uitvoeren), voorwaardelijke breekpunten, en scripting voor geautomatiseerde debuggingtaken.

Hoewel GDB krachtig en veelgebruikt is, kan het dicht en uitdagend zijn voor beginners. Alternatieve debuggingtools en IDE's (Integrated Development Environments) zoals Visual Studio Code, CLion of Eclipse bieden gebruiksvriendelijkere interfaces voor het debuggen van C-code, en integreren vaak visuele hulpmiddelen en intuïtievere bediening. Deze alternatieven bieden misschien niet de volledige diepte van functionaliteit van GDB, maar kunnen toegankelijker zijn voor nieuwkomers in C-programmering.

Bovendien heeft de opkomst van taalserverprotocollen en debuggingsstandaarden de ontwikkeling van platformonafhankelijke debuggingoplossingen vergemakkelijkt, waardoor de debuggingervaring consistenter wordt tussen verschillende tools en omgevingen. Ondanks deze vooruitgang biedt het leren van de ins en outs van een traditionele debugger zoals GDB onschatbaar inzicht in de uitvoering van C-programma's en blijft het een cruciale vaardigheid in de gereedschapskist van een ontwikkelaar.
