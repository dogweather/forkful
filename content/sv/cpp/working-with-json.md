---
title:                "Arbeta med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON (JavaScript Object Notation) hanteras för att förenkla datautbyte. Det är lättläst för människor och lättmaskinläst.

## Hur gör man:
Använd `nlohmann/json` för enkelhet:

```c++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Skapa ett JSON objekt
    nlohmann::json j;
    
    // Lägg till data
    j["namn"] = "Erik";
    j["ålder"] = 30;
    j["programmerar"] = true;

    // Skriv ut JSON som sträng
    std::cout << j.dump(4) << std::endl;
    
    return 0;
}
```

Utskrift:
```
{
    "namn": "Erik",
    "programmerar": true,
    "ålder": 30
}
```

## Fördjupning
JSON kom 2001 för att förenkla datadelning, ursprungligen i JavaScript. Alternativ inkluderar XML och YAML. Implementering i C++ kräver bibliotek som `nlohmann/json` eller `jsoncpp` eftersom standarden inte direkt stöds.

## Se även
- [nlohmann/json på GitHub](https://github.com/nlohmann/json)
- [JSON officiell webbplats](https://www.json.org/json-sv.html)
- [jsoncpp på GitHub](https://github.com/open-source-parsers/jsoncpp)
