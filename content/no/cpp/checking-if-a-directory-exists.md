---
title:                "C++: Forrige sjekk av eksistensen til en mappe"
simple_title:         "Forrige sjekk av eksistensen til en mappe"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med å programmere i C++, er det viktig å kunne sjekke om en mappe eksisterer før man fortsetter kodingen. Dette sikrer at programmet er robust og kan håndtere uventede situasjoner, som for eksempel hvis en nødvendig fil er flyttet eller slettet.

## Hvordan

Det finnes forskjellige metoder for å sjekke om en mappe eksisterer i C++. En av de mest vanlige er å bruke `std::filesystem::exists` funksjonen, som sjekker om en filbane peker mot en eksisterende fil eller mappe. Se eksemplet nedenfor:

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;
  
int main()
{
    // Oppretter en ny filbane som peker mot en eksisterende mappe
    fs::path folderPath = "C:/Brukere/Navn/Dokumenter/";

    // Sjekker om mappen eksisterer
    if (fs::exists(folderPath))
    {
        std::cout << "Mappen eksisterer" << std::endl;
    }
    else
    {
        std::cout << "Mappen eksisterer ikke" << std::endl;
    }
    return 0;
}
```

Eksempelutdata:

```
Mappen eksisterer
```

Det er også mulig å sjekke om en mappe eksisterer ved å bruke `stat` funksjonen, som returnerer informasjon om en fil eller mappe. Hvis det ikke er mulig å hente informasjon om filen eller mappen, betyr det at den ikke eksisterer. Se eksemplet nedenfor:

```C++
#include <iostream>
#include <sys/stat.h>

int main()
{
    // Oppretter en filbane som peker mot en eksisterende mappe
    const char* folderPath = "C:/Brukere/Navn/Dokumenter/";

    // Oppretter en struct for å lagre informasjon om mappen
    struct stat info;

    // Henter informasjon fra mappen og lagrer det i structen
    if(stat(folderPath, &info) != 0) 
    {
        // Hvis mappen ikke eksisterer, vil denne kodesnutten kjøre
        std::cout << "Mappen eksisterer ikke" << std::endl;
    }
    else if(info.st_mode & S_IFDIR) 
    {
        // Hvis mappen eksisterer, vil denne kodesnutten kjøre
        std::cout << "Mappen eksisterer" << std::endl;
    }
    else 
    {
        std::cout << "Mappen eksisterer ikke" << std::endl;
    }

    return 0;
}
```

Eksempelutdata:

```
Mappen eksisterer
```

## Dykk Dypere

Det er viktig å merke seg at når man sjekker om en mappe eksisterer, betyr det ikke nødvendigvis at den er tilgjengelig for å bli brukt. Det kan være flere årsaker til dette, som for eksempel rettigheter eller skrivebeskyttelse. Derfor bør man alltid sjekke at man faktisk kan utføre operasjoner på mappen, og ikke bare at den eksisterer.

Et annet viktig poeng er at hvis man bruker `stat` funksjonen, er det viktig å merke seg hvilken platform man jobber på. Denne metoden fungerer annerledes på forskjellige operativsystemer, og det kan være nødvendig å inkludere forskjellige biblioteker eller bruke andre funksjoner for å få ønsket resultat.

## Se Også

- [std::filesystem::exists dokumentasjon (engelsk)](https://en.cppreference.com/w/cpp/filesystem/exists)
- [stat dokumentasjon (engelsk)](http://man7.org/linux/man-pages/man2/stat.2.html)