---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:13.438456-07:00
description: "Hur: I modern C++ (C++17 och senare) kan du anv\xE4nda filsystembiblioteket\
  \ f\xF6r att kontrollera om en katalog finns. Det erbjuder ett enkelt och\u2026"
lastmod: '2024-03-13T22:44:38.223764-06:00'
model: gpt-4-0125-preview
summary: "I modern C++ (C++17 och senare) kan du anv\xE4nda filsystembiblioteket f\xF6\
  r att kontrollera om en katalog finns."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur:
I modern C++ (C++17 och senare) kan du använda filsystembiblioteket för att kontrollera om en katalog finns. Det erbjuder ett enkelt och standardiserat sätt att utföra filsystemoperationer, inklusive att kontrollera existensen av en katalog.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Katalogen finns." << std::endl;
    } else {
        std::cout << "Katalogen finns inte." << std::endl;
    }

    return 0;
}
```
Exempelutskrift om katalogen finns:
```
Katalogen finns.
```

Exempelutskrift om katalogen inte finns:
```
Katalogen finns inte.
```

För projekt som ännu inte använder C++17 eller för ytterligare funktioner är Boost Filesystem-biblioteket ett populärt tredjepartsval som erbjuder liknande funktionalitet.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Katalogen finns." << std::endl;
    } else {
        std::cout << "Katalogen finns inte." << std::endl;
    }

    return 0;
}
```
Med Boost Filesystem skulle utskriften vara identisk med exemplet för C++17 filsystem, beroende på om katalogen finns på den specificerade sökvägen.
