---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Ett temporärt filskapande innebär att skapa en fil som automatiskt raderas när det inte längre behövs. Programmerare gör detta för att lagra temporära data som inte behöver behållas långsiktigt, vilket sparar minne och optimerar prestanda.

## Hur Man Gör:
Här är ett typisk sätt att skapa och använda en temporär fil i C++. Vi kommer att använda `std::tmpnam` funktionen.

```C++
#include <cstdio>

int main() {
    char* tempPath = std::tmpnam(nullptr);
    FILE* tempFile = std::fopen(tempPath, "w");
    std::fputs("Hello, C++!", tempFile);
    std::fclose(tempFile);
    
    return 0;
}
```
Detta exempel skapar en temporär fil, skriver "Hello, C++!" till den och stänger den. Om du kör det bör du inte se någon utdata, vilket är bra!

## Mera Djupgående:
Sedan tidigare versioner av C++ har funktionen `std::tmpnam` använts för att skapa temporära filer. Den är fortfarande effektiv, men det finns säkrare moderna alternativ som `std::filesystem` i C++17 och framåt som minskar risken för kollisioner eller säkerhetsproblem.

En annan viktig detalj är att platsen och livslängden för temporära filer varierar beroende på vilket operativsystem du använder. I Windows, till exempel, skapas de i användarens Temp-mapp och raderas automatiskt vid omstart.

## Se Även:
- [C++ Documentation - std::tmpnam](https://en.cppreference.com/w/cpp/io/c/tmpnam)
- [C++ Documentation - std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- [Article - Temporary Files](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)