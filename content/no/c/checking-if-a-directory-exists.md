---
title:                "Sjekker om en katalog eksisterer"
html_title:           "C: Sjekker om en katalog eksisterer"
simple_title:         "Sjekker om en katalog eksisterer"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe eksisterer gjør programmering mer dynamisk og feilresistent. Dette muliggjør kodens ustørthet ved å forhindre feil som skjer når man prøver å få tilgang til en ikke-eksisterende mappe.

## Slik gjør du:
I den følgende eksemplet bruker vi `stat()` -funksjonen som sjekker filens tilstand og deretter sjekker om filen er en katalog.

```C
#include <sys/stat.h>

int directory_exists(const char *path) {
   struct stat st;

   if(stat(path, &st) != 0)
       return 0;
   return S_ISDIR(st.st_mode);
}
```

Korrekt implementasjon vil returnere `1` om mappen eksisterer og `0` hvis den ikke gjør det.

## Dypdykk
`stat()` -funksjonen har vært rundt i mange år, og ble først introdusert i UNIX Systems. Det gir detaljert informasjon om filer og er bredt akseptert som den mest pålitelige måten å sjekke om en mappe eksisterer i C.

Alternativt kan du også bruke `opendir()` -funksjonen for å sjekke om en mappe eksisterer, men det er litt langsommere enn `stat()` da det fysisk prøver å åpne mappen.

Implementeringsdetaljer varierer etter systemet. For eksempel, i UNIX-lignende systemer returnerer `stat()` -funksjonen `-1` hvis den angitte banen ikke eksisterer. Derfor, for å sjekke om en katalog eksisterer, ser vi om funksjonen returnerer noe annet enn `-1`. I tillegg ser vi om det er en katalog ved å bruke `S_ISDIR` macro på `st_mode` medlem av `struct stat`.

## Se også
- For mer informasjon om `stat()`, se [man pages](https://man7.org/linux/man-pages/man2/stat.2.html)
- For mer informasjon om `opendir()`, se [man pages](https://www.man7.org/linux/man-pages/man3/opendir.3.html) 
- Hvis du vil dykke dypere ned i filsystem API-er, sjekk ut [APUE](http://www.apuebook.com/) (Advanced Programming in the Unix Environment)