---
title:    "C: Å sjekke om en mappe eksisterer"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er viktig å sjekke om en mappe eksisterer før du prøver å utføre operasjoner med den. Hvis du prøver å åpne eller lagre filer i en mappe som ikke eksisterer, vil programmet ditt kanskje krasje.

## Hvordan Gjøre Det
For å sjekke om en mappe eksisterer i C-programmering, kan du bruke funksjonen `opendir()` som åpner en mappe og returnerer en peker til den. Hvis mappen ikke eksisterer, vil dette returnere `NULL`. Du kan også bruke funksjonen `access()` for å sjekke om den angitte banen (inkludert mappen) er tilgjengelig. Her er et eksempel på hvordan du kan implementere dette i koden din:

```C
#include <stdio.h> 
#include <unistd.h> 
#include <dirent.h> 
  
int main() 
{ 
    // Sjekker om mappen "test" eksisterer 
    if (opendir("test") != NULL) 
        printf("Mappen eksisterer!\n"); 
    else
        printf("Mappen eksisterer ikke.\n"); 
  
    // Sjekker om banen "bilder/bilde.png" er tilgjengelig 
    if (access("bilder/bilde.png", F_OK) != -1) 
        printf("Bildet finnes!\n"); 
    else
        printf("Bildet finnes ikke.\n"); 
  
    return 0; 
} 
```

**Output:**
```
Mappen eksisterer ikke.
Bildet finnes!
```

## Dypdykk
En annen nyttig måte å sjekke om en mappe eksisterer på er å bruke funksjonen `stat()`, som henter informasjon om en fil eller mappe og lagrer den i en `struct`. Hvis funksjonen returnerer `0`, betyr det at mappen eksisterer. Du kan også bruke `mkdir()` for å opprette en mappe hvis den ikke eksisterer, eller `rmdir()` for å slette en mappe hvis den eksisterer. Husk å alltid sjekke returneringsverdiene for å sikre at operasjonene ble utført som forventet.

## Se Også
- [C-programmering: Mappings og Funksjoner](https://www.programiz.com/c-programming/c-struct-function-mapping)
- [The Linux Programming Interface: Åpne og Lese Kataloger](https://man7.org/tlpi/code/online/dist/directories/xstat_lsdir.c.html)