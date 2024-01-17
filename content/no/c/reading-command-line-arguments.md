---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "C: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Lesing av kommandolinje argumenter er en viktig del av å utvikle C-programmer. Dette tillater programmerere å gi instruksjoner til programmet mens det kjører, i stedet for å hardkode dem direkte i kildekoden. Dette gir større fleksibilitet og gjør det enklere å endre programmet uten å måtte endre selve koden.

Hvordan:

For å lese kommandolinje argumenter i C, bruker vi funksjonen `main()` og dens to parametere: `argc` og `argv`. Her er et eksempel på hvordan du kan bruke dette i praksis:

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Antall argumenter: %d\n", argc);

    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

La oss si at vi kjører programmet med følgende kommandolinje argumenter:

```
./program navn alder
```

Da blir følgende resultat:

```
Antall argumenter: 3
Argument 0: ./program
Argument 1: navn
Argument 2: alder
```

Det første argumentet (`argv[0]`) er alltid navnet på programmet.

Deep Dive:

Å lese kommandolinje argumenter har vært en viktig del av programmering siden tidlig på 1970-tallet da C ble utviklet av Dennis Ritchie og Ken Thompson. Det er flere alternative måter å lese argumenter på, for eksempel ved bruk av biblioteker som `getopt` eller ved å lese innholdet i en fil i stedet for kommandolinjen. For å lese argumenter i en mer kompleks programstruktur, kan du bruke `getopt_long` funksjonen.

Implementeringen av å lese kommandolinje argumenter er avhengig av operativsystemet og C-kompilatoren som brukes. Derfor bør du alltid sjekke dokumentasjonen for å sikre at koden din er kompatibel.

Se også:

- [C Programming Language](https://en.wikipedia.org/wiki/C_(programming_language))
- [getopt Long Function](https://www.gnu.org/software/libc/manual/html_node/Using-Getopt-Long.html)