---
title:    "C++: Sjekke om en mappe eksisterer"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er ofte nyttig å sjekke om en mappe eksisterer i et C++ program. Dette kan være nyttig for å håndtere filer og lagre data på en organisert måte. I tillegg kan det bidra til å unngå feil og krasj i programmet. I dette blogginnlegget vil vi utforske hvordan man kan sjekke om en mappe eksisterer i et C++ program.

## Hvordan

Det første vi må gjøre er å inkludere "filesystem" biblioteket i programmet vårt. Dette gjøres ved å bruke "include" kommandoen:

```C++
#include <filesystem>
```

Deretter definerer vi en variabel for mappen vi vil undersøke, for eksempel "myFolder". Vi kan deretter bruke "std::filesystem::exists" funksjonen for å sjekke om denne mappen eksisterer eller ikke. Dette vil returnere en boolsk verdi, som vi kan bruke til å utføre ulike handlinger i programmet vårt. Her er et eksempel på hvordan dette kan se ut:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path myFolder = "/path/to/my/folder";
    if (std::filesystem::exists(myFolder)) {
        std::cout << "Mappen eksisterer!" << std::endl;
    } else {
        std::cout << "Mappen eksisterer ikke!" << std::endl;
    }

    return 0;
}
```

Dette eksempelet vil skrive ut enten "Mappen eksisterer!" eller "Mappen eksisterer ikke!" avhengig av om mappen eksisterer eller ikke.

## Deep Dive

Når vi bruker "std::filesystem::exists" funksjonen, vil det sjekkes om mappen eksisterer og returnere en boolsk verdi. Men hva skjer egentlig bak kulissene? Funksjonen bruker Win32 GetFileAttributes eller POSIX operating system lstat funksjoner for å hente informasjon om en gitt fil eller mappe. Deretter brukes en rekke forskjellige flagg for å bestemme om filen eller mappen eksisterer eller ikke. Dette gir en effektiv måte å sjekke om en mappe eksisterer i et C++ program.

## Se også

- [std::filesystem::exists dokumentasjon](https://en.cppreference.com/w/cpp/filesystem/exists)
- [C++ Filesystem biblioteket](https://en.cppreference.com/w/cpp/filesystem)