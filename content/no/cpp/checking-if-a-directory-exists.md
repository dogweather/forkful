---
title:                "Sjekke om en mappe eksisterer"
html_title:           "C++: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sjekke om en mappe eksisterer er en viktig del av programmering, da det lar deg håndtere potensielle feil i koden din. Dette kan bidra til å forhindre programkrasj og forbedre programytelsen.

## Hvordan
Det finnes flere måter å sjekke om en mappe eksisterer i C++. Her er et eksempel på hvordan du kan gjøre det ved hjelp av `opendir()` og `closedir()` funksjoner:

```C++
#include <iostream>
#include <dirent.h>

using namespace std;

int main() {
  string directory_path = "my_directory";  // endre til relevant mappebane
  DIR* directory = opendir(directory_path.c_str()); // åpner mappen ved hjelp av opendir()
  if (directory) {
    // mappen eksisterer
    closedir(directory); // lukker mappen ved hjelp av closedir()
    cout << "Mappen eksisterer!" << endl;
  }
  else {
    // mappen eksisterer ikke
    cout << "Mappen eksisterer ikke." << endl;
  }
  return 0;
}
```

Output:
```
Mappen eksisterer!
```

## Deep Dive
Merk at `opendir()` og `closedir()` funksjonene tilhører `<dirent.h>` biblioteket, som inneholder funksjoner utviklet for håndtering av kataloger. `opendir()` funksjonen tar inn en mappebane som argument og returnerer en peker til denne mappen hvis den eksisterer, ellers returnerer den en nullpeker. Derfor kan man bruke `if`-setningen til å sjekke om mappen eksisterer eller ikke.

## Se også
- [C++ opendir() function](https://www.cplusplus.com/reference/cstdio/opendir/)
- [C++ closedir() function](https://www.cplusplus.com/reference/cstdio/closedir/)