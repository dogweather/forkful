---
title:                "C: Sjekke om en mappe eksisterer"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger når vi skriver et program, må vi sjekke om en bestemt fil eller mappe eksisterer på datamaskinen vår. Dette kan være nødvendig for å håndtere situasjoner der filen eller mappen er nødvendig for programmet å kjøre, eller for å unngå eventuelle feil eller kaos. Derfor er det viktig å vite hvordan vi kan sjekke om en mappe eksisterer ved hjelp av C-programmering.

## Hvordan

Vi kan sjekke om en mappe eksisterer ved hjelp av C-funksjonen `opendir()`. Denne funksjonen tar inn en mappesti som parameter og returnerer en `DIR` -medlemspeker hvis mappen eksisterer. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```C
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

int main() {
    // Spesifiser mappens sti du vil sjekke
    char *mappe_sti = "/hjem/bruker/testmappe/";

    // Åpne mappen ved hjelp av opendir() og lagre returverdien
    DIR *mappe = opendir(mappe_sti);

    // Sjekk om mappen eksisterer
    if (mappe) {
        printf("Mappen eksisterer!");
        // Husk å lukke mappen igjen når du er ferdig
        closedir(mappe);
    } else {
        printf("Kunne ikke finne mappen!");
    }
    return 0;
}
```

I dette eksemplet bruker vi `opendir()` til å åpne en mappe og lagre returverdien i en `DIR` -medlemspeker. Deretter sjekker vi om denne pekeren er `NULL` eller ikke. Hvis den ikke er `NULL`, betyr det at mappen eksisterer og vi kan utføre de nødvendige operasjonene. Husk å alltid lukke mappen igjen når du er ferdig med å bruke den.

## Deep Dive

Det finnes andre måter å sjekke om en mappe eksisterer på, som for eksempel å bruke funksjonen `stat()` fra `sys/stat.h` biblioteket. Denne funksjonen tar inn en filsti som parameter og returnerer informasjon om filen eller mappen i en `struct` -datatype. Vi kan da sjekke om `st_mode` -medlemmet i `struct` -en har verdien `S_IFDIR` som indikerer at det er en mappe.

Det er også viktig å merke seg at `opendir()` bare sjekker om en mappe eksisterer, det betyr ikke nødvendigvis at mappen er tilgjengelig for skriving eller lesing. Videre undersøkelse ved hjelp av andre funksjoner må gjøres for å sikre at mappen kan brukes som ønsket.

## Se også

- [opendir() official documentation](https://www.man7.org/linux/man-pages/man3/opendir.3.html)
- [stat() official documentation](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html)
- [Sjekk om en mappe eksisterer i C++](https://www.systutorials.com/docs/linux/man/2-opendir/)