---
title:                "Kontrollera om en katalog existerar"
aliases: - /sv/cpp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:13.438456-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns handlar om att avgöra närvaron av en katalog på en specificerad sökväg innan man utför operationer som att läsa från eller skriva till filer inuti den. Programmerare gör detta för att undvika fel relaterade till filoperationer, vilket säkerställer en smidigare och mer tillförlitlig utförande av uppgifter för filhantering i sina applikationer.

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
