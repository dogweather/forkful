---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Slumptalsgenerering är processen att skapa sekvenser av nummer som inte kan förutsägas bättre än av en slumpmässig chans. Programmers använder det för att avleda säkra lösenord, i krypteringsalgoritmer, i spel, för att simulera realistiska händelser i simuleringar och många andra tillämpningar.

## Hur man gör:

Här är ett grundläggande exempel på hur man genererar ett slumptal i C.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    srand(time(0)); // initialisera slumptalsgenerator
    int num = rand(); // generera slumptal
    printf("%d", num);  // Skriv ut slumptalet
    return 0;
}
```

Output kan vara något som detta: `1234567890`

## Fördjupning

För att generera ett slumptal kräver C `rand ()` funktionen, som definieras i stdlib.h biblioteket. För att säkerställa unika nummer varje gång programmet körs anropar vi `srand ()` med den nuvarande tiden som dess frö.

Historiskt sett hade tidiga datorer inga inbyggda metoder för att generera slumptal så programmerare absorberade extern miljödata, som musrörelser, tangentbordsström eller diskdriftstider. C introducerade sedan rand() i sina bibliotek.

Man kan också utforska andra funktioner som `random()`, `drand48()` eller bibliotek som OpenSSL för mer kryptografiskt säkra slumptal. Detta spelar en viktig roll i ställen där säkerheten är avgörande, som lösenordskopplare eller krypteringsalgoritmer.

## Se även

För mer läsning, besök följande länkar:

- [C Standard Library](https://sv.wikipedia.org/wiki/Standardbiblioteket_f%C3%B6r_C)
- [Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)
- [OpenSSL Library](https://www.openssl.org/)