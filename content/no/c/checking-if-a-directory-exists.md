---
title:                "C: Å sjekke om en mappe eksisterer."
simple_title:         "Å sjekke om en mappe eksisterer."
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å sjekke om en mappe eksisterer kan være nyttig for å sikre at programmet vårt kjører som forventet. Dette gjør det mulig å håndtere situasjoner der en mappe ikke er tilgjengelig, og dermed unngå feil eller krasj i programmet vårt.

## Hvordan gjøre det

For å sjekke om en mappe eksisterer, kan vi bruke `opendir()` funksjonen fra `dirent.h` biblioteket i C. Dette vil åpne mappen vi ønsker å sjekke og returnere en peker til denne mappen. Om mappen ikke finnes, vil `opendir()` returnere `NULL`.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Sjekker om mappen "test" eksisterer
    DIR *dir = opendir("test"); 
    if (dir == NULL) {
        printf("Mappen finnes ikke.\n");
    } else {
        printf("Mappen finnes.\n");
        closedir(dir); // Husk å lukke mappen etter at vi er ferdig med den
    }
    return 0;
}
```

### Output:

```
Mappen finnes ikke.
```

## Dypdykk

En ting å merke seg når vi sjekker om en mappe eksisterer er at dette ikke nødvendigvis betyr at vi har tilgang til den. For å sjekke om vi har tilgang til en mappe, kan vi bruke `access()` funksjonen fra `unistd.h` biblioteket. Denne funksjonen tar inn et filnavn og en liste av tillatelser vi ønsker å sjekke for, og returnerer 0 om vi har tilgang til mappen og -1 om vi ikke har det.

```C
#include <stdio.h>
#include <unistd.h>

int main() {
    // Sjekker om vi har skriverettigheter til mappen "test"
    if (access("test", W_OK) == 0) {
        printf("Vi har skriverettigheter til mappen.\n");
    } else {
        printf("Vi har ikke skriverettigheter til mappen.\n");
    }
    return 0;
}
```

### Output:

```
Vi har skriverettigheter til mappen.
```

Vi kan også gå et steg videre og sjekke om en fil eksisterer innenfor en mappe ved å kombinere `opendir()` og `access()` funksjonene. Dette kan være nyttig når vi ønsker å sjekke om en konfigurasjonsfil eller en databasefil eksisterer før vi prøver å åpne den.

For å lære mer om andre måter å håndtere mapper og filer i C, anbefales det å lese dokumentasjonen for `dirent.h` og `unistd.h` bibliotekene.

## Se også

- [dirent.h dokumentasjon](https://www.ibm.com/docs/en/ad/v12r1?topic=functions-direnth)
- [unistd.h dokumentasjon](https://www.gnu.org/software/libc/manual/html_node/Searching-Directory-Tree.html)