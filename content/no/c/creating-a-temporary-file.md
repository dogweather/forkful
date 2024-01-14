---
title:                "C: Opprettelse av midlertidig fil"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#Hvorfor

Vi har alle opplevd det før - behovet for å lage en midlertidig fil mens vi koder i C. Dette kan være nyttig for å lagre midlertidig data eller for å kommunisere med andre programmer. Uansett årsak, er det viktig å forstå hvordan man oppretter en midlertidig fil i C for å kunne lykkes som programmerer.

#Hvordan

Å opprette en midlertidig fil i C er ganske enkelt, og det krever bare noen få linjer med kode. For å gjøre dette må vi inkludere <stdio.h> biblioteket, som inneholder funksjonen "tmpfile()" som vi skal bruke.

```C
#include <stdio.h>

int main() {

    // Setter opp en midlertidig peker til filen
    FILE *fp;

    // Bruker tmpfile() for å opprette en midlertidig fil
    fp = tmpfile();

    // Sjekker om opprettelsen av filen var vellykket
    if(fp == NULL) {
        printf("Kunne ikke opprette midlertidig fil\n");
        return 1;
    }

    // Skriver til filen
    fputs("Dette er en midlertidig fil", fp);

    // Lukker filen
    fclose(fp);

    return 0;
}
```

Når vi kjører denne koden, vil det opprettes en midlertidig fil som vil inneholde teksten "Dette er en midlertidig fil". Vi kan også bruke andre filbehandlingsfunksjoner, som for eksempel "fprintf()", til å skrive mer komplekse data til filen.

#Dypdykk

Hvordan fungerer egentlig "tmpfile()" funksjonen? Når vi kaller denne funksjonen, vil det bli opprettet en midlertidig fil i det midlertidige filsystemet på datamaskinen vår. Dette filsystemet er en del av operativsystemet som brukes til å lagre midlertidige filer, og vil bli slettet når datamaskinen blir restartet.

Det er også verdt å merke seg at en midlertidig fil vil ha en unik filnavn og kan ikke åpnes av andre programmer mens den er i bruk. Når filen blir lukket, blir den automatisk slettet fra systemet.

#Se også

- [tmpfile() funksjonen i C](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Mer informasjon om det midlertidige filsystemet](https://en.wikipedia.org/wiki/Tmpfs)