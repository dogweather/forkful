---
title:                "Sjekke om en mappe eksisterer"
html_title:           "C: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Du lurer kanskje på hvorfor det er viktig å sjekke om en mappe eksisterer i et C-program. Det er fordi det kan hjelpe deg med å sikre at programmet ditt kjører riktig og forhindrer eventuelle feil eller krasj.

## Hvordan gjøre det

Det første trinnet er å inkludere "dirent.h" biblioteket i programmet ditt ved å bruke "```#include <dirent.h>```" kommandoen. Dette biblioteket inneholder funksjoner som lar deg jobbe med mapper og filer.

Deretter bruker du "opendir()" funksjonen for å åpne mappen du vil sjekke. Denne funksjonen tar imot navnet på mappen som en streng og returnerer en peker til denne mappen hvis den eksisterer. Hvis mappen ikke eksisterer, vil den returnere NULL.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Sjekk om mappen "test" eksisterer
    DIR *folder = opendir("test");

    // Sjekk om mappen ble åpnet
    if (folder != NULL) {
        printf("Mappen eksisterer!\n");
        // Gjør andre handlinger her
    } else {
        printf("Mappen eksisterer ikke!\n");
    }

    // Lukk mappen
    closedir(folder);

    return 0;
}
```
Eksempelutgang:

```
Mappen eksisterer ikke!
```

## Dypdykk

"opendir()" funksjonen er en del av POSIX-standardene, noe som betyr at den vil fungere på de fleste Unix-like systemer, som Linux og Mac OS. Denne funksjonen tar også imot en absolutt eller relativ filsti som argument, så du kan sjekke mapper uavhengig av hvor programmet ditt er plassert.

En annen nyttig funksjon er "stat()" som lar deg hente informasjon om en fil eller mappe. Du kan da bruke denne informasjonen til å bekrefte at en angitt fil eller mappe faktisk eksisterer. For mer informasjon om denne funksjonen, kan du lese dokumentasjonen her: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html)

## Se også

- [https://www.tutorialspoint.com/c_standard_library/index.htm](https://www.tutorialspoint.com/c_standard_library/index.htm)
- [https://www.programiz.com/c-programming/c-dirent-h](https://www.programiz.com/c-programming/c-dirent-h)
- [https://www.ibm.com/docs/en/aix/7.1?topic=g-makedev-testing-pathname-existing](https://www.ibm.com/docs/en/aix/7.1?topic=g-makedev-testing-pathname-existing)