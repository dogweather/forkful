---
title:                "Att arbeta med yaml"
html_title:           "C++: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför
YAML är ett lättläst och användarvänligt sätt att strukturera och lagra data i ett programmeringsprojekt. Det är ett populärt verktyg för att hantera konfigurationsfiler och definiera datastrukturer.

## Så här
För att använda YAML i ditt C++-projekt behöver du först inkludera YAML-biblioteket. Detta görs genom att lägga till följande rad i din kod:
```C++
#include <yaml-cpp/yaml.h>
```
Sedan kan du läsa in och skriva ut YAML-filer med hjälp av följande kodexempel:
```C++
YAML::Node data = YAML::LoadFile("file.yaml"); // Läser in innehållet från filen "file.yaml"
std::cout << data["key1"]; // Skriver ut värdet för "key1" i filen
```
För att skapa en YAML-fil och skriva data till den kan du använda följande kod:
```C++
YAML::Emitter emitter; // Skapar en emitter för att skriva YAML
emitter << YAML::BeginMap; // Börjar en mapp
emitter << YAML::Key << "key1"; // Definierar en nyckel
emitter << YAML::Value << "value1"; // Definierar ett värde
emitter << YAML::EndMap; // Avslutar mappen
std::ofstream file("file.yaml"); // Öppnar filen för skrivning
file << emitter.c_str(); // Skriver ut emittern till filen
```

## Deep Dive
YAML stödjer flera datatyper från standard C++, såsom strängar, tal, boolska värden och arrayer. Det finns också stöd för mer avancerade datatyper som kan definieras i koden. 

Det är också möjligt att inkludera andra YAML-filer inuti ditt YAML-dokument. Detta kan vara användbart för att återanvända data och för att hålla din YAML-fil mer strukturerad och lättläst.

En annan fördel med YAML är dess flexibilitet och förmåga att anpassa sig till olika strukturer och datatyper. Det finns också möjlighet att kommentera din YAML-kod för att ge ytterligare förklaringar och förbättra läsbarheten.

## Se även
- [YAML-bibliotekets officiella hemsida](https://yaml-cpp.github.io/)
- [En enkel guide till YAML](https://rollbot.net/yaml-guide/)
- [C++ - En introduktion](https://www.cplusplus.com/)