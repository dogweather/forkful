---
title:                "Håndtering av feil"
date:                  2024-01-26T00:37:14.698283-07:00
model:                 gpt-4-1106-preview
simple_title:         "Håndtering av feil"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Håndtering av feil i C handler om å forvente det uventede. Det forhindrer programmer i å gå haywire når de støter på problemer. Programmerere gjør dette for å håndtere feil på en elegant måte og for å holde koden pålitelig.

## Hvordan gjøre:

La oss se hvordan man gjør dette i C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("Feil ved åpning av fil");
        return EXIT_FAILURE;
    }
    // Gjør noe med filen
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Eksempel på output når filen ikke eksisterer:
```
Feil ved åpning av fil: Ingen slik fil eller katalog
```

## Dypdykk

I C's tidlige dager var feilhåndtering enkel - stort sett returkoder og manuelle sjekker. Så kom `errno`, en global variabel som oppdateres når funksjoner feiler. Den er imidlertid ikke trådsikker i seg selv, så de nyere funksjonene `strerror` og `perror` ble introdusert for bedre feilrapportering.

Alternativer? Moderne C er ikke begrenset til `errno`. Der er setjmp og longjmp for ikke-lokale hopp når katastrofen inntreffer. Noen foretrekker å definere sine egne feilkoder, mens andre velger for unntakslignende strukturer i C++.

Implementasjonsdetaljer kan være komplekse. For eksempel er `errno` trådsikker i POSIX-kompatible systemer på grunn av magien med Thread Local Storage (TLS). I innebygde systemer, hvor ressurser er dyrebare, kan tilpasset feilhåndteringskode være å foretrekke foran standardtilnærminger som kan blåse opp programvaren.

## Se også

- En detaljert dykk inn i `errno`: https://en.cppreference.com/w/c/error/errno
- For trådsikkerhet, se POSIX-tråder og errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- En introduksjon til setjmp og longjmp: https://www.cplusplus.com/reference/csetjmp/
- For unntakshåndtering i C++, sjekk ut: https://isocpp.org/wiki/faq/exceptions
