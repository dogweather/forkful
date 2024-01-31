---
title:                "Att använda associativa arrayer"
date:                  2024-01-30T19:10:17.268752-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda associativa arrayer"
programming_language: "C"
category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Associativa fält, eller hashkartor, är nyckel-värde-par som låter dig lagra och hämta data med en nyckel. De är otroligt användbara i C eftersom de möjliggör snabbare dataåtkomst jämfört med listor, speciellt när du hanterar en stor mängd data.

## Hur man gör:

C har inte inbyggt stöd för associativa fält som vissa andra språk, men vi kan använda strukturer och några biblioteksfunktioner för att få liknande funktionalitet. Här är en enkel implementering som använder `uthash`-biblioteket, vilket du behöver inkludera i ditt projekt.

Först, definiera en struktur för att hålla dina nyckel-värde-par:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Det här blir vår nyckel
    char name[10]; // Det här är värdet som är associerat med vår nyckel
    UT_hash_handle hh; // Gör denna struktur hashbar
} person;
```

Nästa, låt oss lägga till några poster och hämta dem:

```C
int main() {
    person *my_people = NULL, *s;

    // Lägger till en post
    s = (person*)malloc(sizeof(person));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(my_people, id, s);

    // Hämtar en post
    int user_id = 1;
    HASH_FIND_INT(my_people, &user_id, s);
    if (s) {
        printf("Hittad: %s\n", s->name);
    }
    
    return 0;
}
```

Exempel på utdata skulle vara:

```
Hittad: Alice
```

Glöm inte att frigöra allokerat minne och deallokera hashtabellen när du är klar för att undvika minnesläckor.

## Fördjupning

Även om associativa fält inte är inbyggda i C, fyller bibliotek som `uthash` gapet ganska bra, och tillhandahåller ett relativt okomplicerat sätt att använda denna funktionalitet. Historiskt sett har C-utvecklare behövt implementera sina versioner av dessa datastrukturer, vilket har lett till varierade och ofta komplexa implementeringar, speciellt för de som precis börjat med språket.

Kom ihåg, effektiviteten av att använda associativa fält i C beror mycket på hur väl hashfunktionen distribuerar värden över tabellen för att minimera kollisioner. Även om bibliotek som `uthash` erbjuder en bra balans mellan användarvänlighet och prestanda, kan det i kritiska applikationer där prestanda är av yttersta vikt, vara bättre att anpassa eller implementera din egen hashtabell.

För applikationer som kräver maximal effektivitet kan alternativa datastrukturer eller till och med andra programmeringsspråk med inbyggt stöd för associativa fält vara ett bättre val. Dock, i många situationer, speciellt där du redan arbetar inom en C-miljö, erbjuder användning av ett bibliotek som `uthash` en praktisk balans mellan prestanda och bekvämlighet.
