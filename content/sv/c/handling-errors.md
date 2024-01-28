---
title:                "Hantering av fel"
date:                  2024-01-26T00:49:28.480737-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hantering av fel"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/handling-errors.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hantera fel i C handlar om att förvänta sig det oväntade. Det förhindrar program från att spåra ur när de stöter på problem. Programmerare gör detta för att hantera misstag på ett smidigt sätt och för att hålla sin kod tillförlitlig.

## Hur man gör:

Låt oss se hur man gör detta i C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("obefintligfil.txt", "r");
    if (fp == NULL) {
        perror("Fel vid öppning av fil");
        return EXIT_FAILURE;
    }
    // Gör något med filen
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Exempelutdata när filen inte finns:
```
Fel vid öppning av fil: Ingen sådan fil eller katalog
```

## Fördjupning

I C:s tidiga dagar var felsökningen grundläggande - mestadels returkoder och manuella kontroller. Entré `errno`, en global variabel som uppdateras när funktioner misslyckas. Den är dock inte trådsäker i sig själv, så de nyare funktionerna `strerror` och `perror` introducerades för bättre felrapportering.

Alternativ? Modern C är inte begränsad till `errno`. Det finns `setjmp` och `longjmp` för icke-lokala hopp när katastrofen är framme. En del föredrar att definiera sina egna felfunktioner, medan andra optar för exception-liknande strukturer i C++.

Implementeringsdetaljerna kan vara komplicerade. Till exempel är `errno` trådsäkert i POSIX-kompatibla system tack vare magin med Thread Local Storage (TLS). I inbyggda system, där resurserna är dyrbart, kan personlig felsökningskod föredras över standardmetoder som kan svälla mjukvaran.

## Se även

- En detaljerad dykning i `errno`: https://en.cppreference.com/w/c/error/errno
- För trådsäkerhet, se POSIX-trådar och errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- En introduktion till setjmp och longjmp: https://www.cplusplus.com/reference/csetjmp/
- För exception-hantering i C++, se: https://isocpp.org/wiki/faq/exceptions
