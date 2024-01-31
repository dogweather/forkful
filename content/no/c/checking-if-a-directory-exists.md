---
title:                "Sjekke om en mappe finnes"
date:                  2024-01-19
html_title:           "Arduino: Sjekke om en mappe finnes"
simple_title:         "Sjekke om en mappe finnes"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekke om en mappe eksisterer er å verifisere at en spesifikk mappe faktisk finnes i filsystemet. Programmerere gjør dette for å unngå feil ved filoperasjoner, som å lese fra eller skrive til en ikke-eksisterende mappe.

## Slik gjør du:
Bruk `stat`-strukturen og `opendir` fra `<sys/stat.h>` og `<dirent.h>` bibliotekene for å sjekke mappen. Her er et eksempel:

```c
#include <stdio.h>
#include <sys/stat.h>
#include <dirent.h>

int directory_exists(const char *path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != 0) return 0; // Kunne ikke få status, anta at den ikke eksisterer
    return S_ISDIR(statbuf.st_mode); // Sjekk om det er en mappe
}

int main() {
    const char *path = "/min/mappe";
    if (directory_exists(path)) {
        printf("Mappen '%s' eksisterer.\n", path);
    } else {
        printf("Mappen '%s' finnes ikke.\n", path);
    }
    return 0;
}
```
Mulig utskrift kan være:
```
Mappen '/min/mappe' eksisterer.
```
Eller:
```
Mappen '/min/mappe' finnes ikke.
```

## Dypdykk
Å sjekke for eksistensen av en mappe har vært en del av programmeringsrutinene siden tidlige dager av Unix. Den tradisjonelle `stat`-funksjonen gir detaljert informasjon om filattributter, som kan brukes til å sjekke om en filsti korresponderer til en mappe. Selv om `stat` er effektiv, kan den møte race conditions, hvor mappens status kan endre seg mellom sjekken og den etterfølgende operasjonen.

Alternativt, `opendir()` fra `<dirent.h>` sjekker også eksistensen av en mappe og er mer rettet mot katalogoperasjoner. En annen vanlig tilnærming på noen systemer er å bruke `access()` eller `faccessat()` med `R_OK` for å sjekke lesbarhet.

I programmeringssammenheng må vi alltid vurdere både portabilitet og ytelse. Noen metoder kan være mer bærekraftige på tvers av forskjellige plattformer, mens andre kan være raskere, men mindre kompatible.

## Se også
- POSIX standarden for filsystemfunksjoner: http://pubs.opengroup.org/onlinepubs/9699919799/
- C Standard Library dokumentasjon for `<sys/stat.h>`: https://en.cppreference.com/w/c/io
- GNU C Library Manual for Directory Access: https://www.gnu.org/software/libc/manual/html_node/Directory-Access.html
