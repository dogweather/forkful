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

## Hva og Hvorfor?
Sjekke om en mappe eksisterer er en viktig del av programmering. Det lar deg kontrollere om en spesifisert mappe faktisk finnes på datamaskinen din. Dette kan være nyttig når du søker etter filer eller ønsker å utføre handlinger bare hvis mappen eksisterer.

## Hvordan:
For å sjekke om en mappe eksisterer, kan du bruke følgende kode i ditt C-program:

```C
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h> // biblioteket for mappebehandling

int main()
{
    // Sett opp variabler
    DIR *mappe;
    char *navn = "min_mappe";
    
    // Åpne mappen med navnet "min_mappe" i gjeldende katalog
    mappe = opendir(navn);
    
    // Sjekk om mappen eksisterer
    if(mappe) {
        printf("%s eksisterer!\n", navn);
        closedir(mappe); // lukk mappen for å unngå problemer
    } else {
        printf("%s eksisterer ikke.\n", navn);
    }
    
    return 0;
}
```

Dette eksempelet bruker funksjoner fra dirent.h biblioteket, som er spesialisert for mappebehandling i C-programmer.

Kjører du dette programmet, vil du få følgende utskrift:
```
min_mappe eksisterer ikke.
```

## Dypdykk:
I tidligere versjoner av C, måtte man bruke system-kommandoer som "stat" eller "access" for å sjekke om en mappe eksisterer. Dette kunne føre til sikkerhetsrisikoer, spesielt når en bruker brukeren innmatingsverdier.

I nyere versjoner av C, som den som brukes i dette eksempelet, finnes det innebygde funksjoner for å lett sjekke om en mappe eksisterer.

Det finnes også andre alternativer for mappebehandling i C, som for eksempel "opendir" og "readdir" funksjonene som brukes i dette eksempelet. Disse gir enkle måter å navigere og arbeide med mapper i C-programmer.

## Se også:
- [The Dirent.h Header File in C](https://www.tutorialspoint.com/c_standard_library/dirent_h.htm)
- [C Programming Tutorial: Directory handling](https://www.techwalla.com/articles/c-programming-tutorial-directory-handling)