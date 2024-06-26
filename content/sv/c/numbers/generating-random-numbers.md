---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:23.494405-07:00
description: "Hur man g\xF6r: I C kan slumpm\xE4ssiga nummer genereras med funktionen\
  \ `rand()`, som \xE4r en del av C:s standardbibliotek `<stdlib.h>`. Som standard\
  \ producerar\u2026"
lastmod: '2024-03-13T22:44:38.377824-06:00'
model: gpt-4-0125-preview
summary: "I C kan slumpm\xE4ssiga nummer genereras med funktionen `rand()`, som \xE4\
  r en del av C:s standardbibliotek `<stdlib.h>`."
title: "Generera slumpm\xE4ssiga nummer"
weight: 12
---

## Hur man gör:
I C kan slumpmässiga nummer genereras med funktionen `rand()`, som är en del av C:s standardbibliotek `<stdlib.h>`. Som standard producerar `rand()` pseudoslumpmässiga nummer i intervallet från 0 till `RAND_MAX` (en konstant definierad i `<stdlib.h>`). För mer kontroll över intervallet kan programmerare manipulera utmatningen från `rand()`.

Här är ett enkelt exempel på att generera ett slumpmässigt nummer mellan 0 och 99:

```c
#include <stdio.h>
#include <stdlib.h> // För rand() och srand()
#include <time.h>   // För time()

int main() {
    // Så kärnan för slumpmässig nummergenerator
    srand((unsigned) time(NULL));

    // Generera ett slumpmässigt nummer mellan 0 och 99
    int slumpmassigtNummer = rand() % 100;

    printf("Slumpmässigt nummer: %d\n", slumpmassigtNummer);

    return 0;
}
```

Exempel på utmatning kan variera varje gång du kör detta program:

```
Slumpmässigt nummer: 42
```
För att generera slumpmässiga nummer inom ett annat intervall kan du justera modulusoperatorn (`%`) därefter. Till exempel genererar `rand() % 10` nummer från 0 till 9.

Det är viktigt att notera att såddning av den pseudoslumpmässiga nummergeneratorn (`srand()`-anropet) med aktuell tid (`time(NULL)`) säkerställer olika sekvenser av slumpmässiga nummer över programkörningar. Utan såddning (`srand()`), skulle `rand()` producera samma sekvens av nummer varje gång programmet körs.

## Djupdykning
Funktionen `rand()` och dess såddningsmotsvarighet `srand()` har varit en del av C:s standardbibliotek i årtionden. De är baserade på algoritmer som genererar sekvenser av nummer som enbart verkar vara slumpmässiga - därav termen "pseudoslumpmässig." Den underliggande algoritmen i `rand()` är vanligtvis en linjär kongruentiell generator (LCG).

Även om `rand()` och `srand()` är tillräckliga för många applikationer, har de kända begränsningar, särskilt gällande kvaliteten på slumpmässigheten och potentiell förutsägbarhet. För applikationer som kräver högkvalitativ slumpmässighet, såsom kryptografiska operationer, bör alternativ som `/dev/random` eller `/dev/urandom` (på Unix-liknande system), eller API:er som tillhandahålls av kryptografiska bibliotek, övervägas.

Med introduktionen av C11 inkluderade ISO C-standard ett nytt huvud, `<stdatomic.h>`, som erbjuder en mer finjusterad kontroll för samtidiga operationer, men inte direkt rörande slumpmässighet. För verklig slumpmässighet i C vänder sig utvecklare ofta till plattformsspecifika eller externa bibliotek som erbjuder bättre algoritmer eller utnyttjar hårdvaru-entropikällor.

Kom ihåg, medan `rand()` fungerar som ett enkelt och tillgängligt sätt att generera pseudoslumpmässiga nummer, är dess användning i moderna applikationer begränsad av kvaliteten och förutsägbarheten i dess utmatning. När mer robusta lösningar krävs, särskilt för säkerhetsmedvetna applikationer, är det starkt rekommenderat att utforska bortom standardbiblioteket.
