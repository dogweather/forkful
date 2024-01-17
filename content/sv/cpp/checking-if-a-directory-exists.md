---
title:                "Kontrollera om en mapp finns"
html_title:           "C++: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp existerar är en process som programmerare använder för att försäkra sig om att en viss mapp eller sökväg finns på en dator eller server. Detta är ett viktigt steg för att säkerställa att programmet kan fortsätta köra utan problem.

## Hur man:
Du kan använda C++ biblioteket **<filesystem>** för att kontrollera om en mapp finns eller inte. Här är ett exempel på hur man kan göra det:

```
#include <iostream>
#include <filesystem>
    
int main() {
    if(std::filesystem::exists("C:/Users/mappnamn")) {
        std::cout << "Mappen finns!" << std::endl;
    } else {
        std::cout << "Mappen finns inte!" << std::endl;
    }
    return 0;
}
```

**Output:** Mappen finns eller Mappen finns inte beroende på om mappen existerar eller inte.

## Deep Dive:
Att kontrollera en mappexister är ett vanligt behov för många olika program, speciellt för program som hanterar filer eller mappar på en dator. Alternativet att använda **<filesystem>** biblioteket i C++ är en enkel och effektiv metod för att lösa denna uppgift.

Det finns också andra metoder för att kontrollera om en mapp existerar i C++, som till exempel att använda **_stat** funktionen eller **GetFileAttributes** funktionen från Windows API. Dessa metoder kräver emellertid mer kod och kan vara svårare att använda än att helt enkelt använda **<filesystem>** biblioteket.

När du använder **<filesystem>** biblioteket för att kontrollera om en mapp existerar görs detta genom att söka igenom filsystemet efter den givna sökvägen. Om mappen finns kommer funktionen att returnera **true**, annars kommer den att returnera **false**.

## Se även:
- [std::filesystem::exists](https://en.cppreference.com/w/cpp/filesystem/exists)
- [_stat function](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/stat-wstat-_stat32-wstat32)
- [GetFileAttributes function](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesa)