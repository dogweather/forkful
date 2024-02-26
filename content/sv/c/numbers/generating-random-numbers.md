---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:23.494405-07:00
description: "Att generera slumpm\xE4ssiga nummer i C inneb\xE4r att skapa v\xE4rden\
  \ som \xE4r of\xF6ruts\xE4gbara och f\xF6ljer en specifik distribution, s\xE5som\
  \ uniform eller normal. Denna\u2026"
lastmod: '2024-02-25T18:49:36.693019-07:00'
model: gpt-4-0125-preview
summary: "Att generera slumpm\xE4ssiga nummer i C inneb\xE4r att skapa v\xE4rden som\
  \ \xE4r of\xF6ruts\xE4gbara och f\xF6ljer en specifik distribution, s\xE5som uniform\
  \ eller normal. Denna\u2026"
title: "Generera slumpm\xE4ssiga nummer"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga nummer i C innebär att skapa värden som är oförutsägbara och följer en specifik distribution, såsom uniform eller normal. Denna förmåga är avgörande för applikationer som sträcker sig från simuleringar och spel till kryptografiska operationer, där oförutsägbarhet eller simulering av verklig slumpmässighet är essentiell.

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
