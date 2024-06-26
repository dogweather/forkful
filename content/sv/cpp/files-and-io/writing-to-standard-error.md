---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:40.595452-07:00
description: "Hur man g\xF6r: I C++ kan skrivning till standardfel uppn\xE5s genom\
  \ att anv\xE4nda str\xF6mmen `cerr`, som \xE4r en del av standardbiblioteket. H\xE4\
  r \xE4r ett grundl\xE4ggande\u2026"
lastmod: '2024-03-13T22:44:38.225752-06:00'
model: gpt-4-0125-preview
summary: "I C++ kan skrivning till standardfel uppn\xE5s genom att anv\xE4nda str\xF6\
  mmen `cerr`, som \xE4r en del av standardbiblioteket."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
I C++ kan skrivning till standardfel uppnås genom att använda strömmen `cerr`, som är en del av standardbiblioteket. Här är ett grundläggande exempel:

```cpp
#include <iostream>

int main() {
    // Skrivning till standardutdata
    std::cout << "Detta är ett vanligt meddelande." << std::endl;
    
    // Skrivning till standardfel
    std::cerr << "Detta är ett felmeddelande." << std::endl;
    
    return 0;
}
```

Exempel på utdata:
```
Detta är ett vanligt meddelande.
Detta är ett felmeddelande.
```

I detta fall kommer båda meddelandena typiskt att visas i din terminal, men du kan omdirigera dem separat i ett skal. Till exempel kan du skicka standardutdata till en fil medan du låter fel visas på skärmen.

För mer avancerad loggning och felhantering kan tredjepartsbibliotek som `spdlog` eller `boost.log` användas. Dessa bibliotek erbjuder förbättrade funktioner för loggning, inklusive formatering, loggnivåer och filutdata.

Så här kan du använda `spdlog` för att skriva ett felmeddelande:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // Initialisera spdlog
    spdlog::info("Detta är ett vanligt meddelande.");
    spdlog::error("Detta är ett felmeddelande.");
    
    return 0;
}
```

Obs: För att använda `spdlog` måste du lägga till det i ditt projekt. Du kan göra detta genom att klona repositoryt från GitHub eller använda en pakethanterare som `vcpkg` eller `conan`.

Kom ihåg att valet mellan att direkt använda standardströmmar eller ett bibliotek som `spdlog` beror på komplexiteten i din applikation och dina specifika behov med avseende på felhantering och loggning.
