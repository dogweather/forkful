---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:36.688470-07:00
description: "Hoe te: Vanaf C++17 hebben we `std::filesystem` om ons leven makkelijker\
  \ te maken voor bestandssysteemoperaties. Hier is een codefragment om te\u2026"
lastmod: '2024-03-13T22:44:51.127446-06:00'
model: gpt-4-0125-preview
summary: Vanaf C++17 hebben we `std::filesystem` om ons leven makkelijker te maken
  voor bestandssysteemoperaties.
title: Controleren of een directory bestaat
weight: 20
---

## Hoe te:
Vanaf C++17 hebben we `std::filesystem` om ons leven makkelijker te maken voor bestandssysteemoperaties. Hier is een codefragment om te controleren of een directory bestaat:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dir_path{"./some_directory"};

    bool exists = std::filesystem::exists(dir_path);
    if(exists) {
        std::cout << "Directory bestaat." << std::endl;
    } else {
        std::cout << "Directory bestaat niet." << std::endl;
    }

    return 0;
}
```

Voorbeelduitvoer (als de directory bestaat):
```
Directory bestaat.
```

Of (als de directory niet bestaat):
```
Directory bestaat niet.
```

## Diepgaand
Voor C++17 moesten we vertrouwen op platformspecifieke API-aanroepen of externe bibliotheken. In de Windows API hadden we `GetFileAttributes` kunnen gebruiken en controleren of de retourwaarde `INVALID_FILE_ATTRIBUTES` was. Op POSIX-systemen konden we de `stat()` functie gebruiken voor een soortgelijke functionaliteit.

C++17 veranderde het spel met `std::filesystem`. Het biedt ondersteuning voor meerdere platformen en een hoogwaardige interface om te interageren met het bestandssysteem. De `exists()` functie is de directe manier om te controleren op de aanwezigheid van een directory, maar je kunt ook `is_directory()` gebruiken als je niet alleen de aanwezigheid wilt bevestigen, maar ook dat het pad naar een directory wijst en niet naar een bestand.

Voor alternatieve methoden, overweeg de `std::filesystem::status_known()` en `std::filesystem::file_status` functies om gevallen te behandelen waarbij bestandsrechten of andere kwesties je mogelijkheid kunnen beïnvloeden om te bepalen of een directory bestaat.

## Zie Ook
Verken meer over bestandssysteemoperaties in C++:

- [std::filesystem documentatie](https://en.cppreference.com/w/cpp/filesystem)
- Voor historische context en verschillen tussen versies, zie [C++ versiegeschiedenis](https://en.cppreference.com/w/cpp/compiler_support)
