---
title:                "Hente nåværende dato"
html_title:           "C: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen er ofte en viktig del av mange programmeringsoppgaver. Det kan være for å registrere tidsstempling på handlinger, generere rapporter eller for å holde styr på tidsbaserte hendelser.

## Hvordan

For å få den nåværende datoen i C, kan du bruke funksjonen "time()" som returnerer antall sekunder siden epoken, også kjent som Unix-tiden. Du kan deretter bruke "localtime()" for å konvertere dette til en mer lesbar datoformat.

For eksempel:

```C
// Inkluderer nødvendige biblioteker
#include <stdio.h>
#include <time.h>

int main(void) {
    // Henter tiden på nåværende øyeblikk
    time_t now = time(NULL);
    
    // Konverterer til et mer leselig format
    struct tm *date = localtime(&now);
    
    // Skriver ut datoen i ønsket format
    printf("Dagens dato er %02d/%02d/%d", date->tm_mday, date->tm_mon + 1, date->tm_year + 1900);

    return 0;
}
```

Dette vil gi output som: "Dagens dato er 01/02/2021".

## Deep Dive

Funksjonene som brukes for å få den nåværende datoen, "time()" og "localtime()", stammer fra standardbiblioteket "time.h". "time()" returnerer antall sekunder siden "epoken", som er en spesifikk tid som brukes som referansepunkt for å beregne tiden på datamaskiner. Denne tiden varierer avhengig av operativsystemet, men på Unix-systemer er den vanligvis satt til 1. januar 1970 klokken 00:00.

"localtime()" bruker dette antallet sekunder til å konvertere til en "struct tm" som inneholder informasjon om dato og tid i en leselig form. Denne informasjonen kan fås ved å bruke ulike "struct tm" medlemmer, for eksempel "tm_mday" for dagen i måneden og "tm_mon" for månedens nummer, der januar er 0 og desember er 11.

I tillegg til disse funksjonene, finnes det også andre måter å få tak i den nåværende datoen på, for eksempel ved hjelp av biblioteket "ctime" som inneholder funksjoner som "ctime()" som returnerer en strengrepresentasjon av den lokale tiden.

## Se også

- [C time manpage](https://linux.die.net/man/3/time)
- [C localtime manpage](https://linux.die.net/man/3/localtime)
- [Ctime function in C](https://www.tutorialspoint.com/c_standard_library/c_function_ctime.htm)