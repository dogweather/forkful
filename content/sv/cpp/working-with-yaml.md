---
title:                "C++: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Om du arbetar med att programmera i C++ så är du säkert bekant med olika typer av datastrukturer som används för att lagra och organisera data. En typ av datastruktur som ofta används är YAML, som står för "YAML Ain't Markup Language". Den är användbar för att lagra data i en strukturerad och läsbar form. I denna bloggpost kommer vi att titta närmare på varför det är viktigt att lära sig arbeta med YAML och hur man kan implementera det i en C++ miljö.

## Så här

### Kodexempel
För att använda YAML i en C++ miljö, börja med att inkludera ```yaml-cpp``` biblioteket och skapa ett YAML dokument med följande kod:
```C++
#include <yaml-cpp/yaml.h>

YAML::Node doc = YAML::LoadFile("example.yaml"); // Ladda in YAML-dokumentet
```
Vi kan sedan få tillgång till olika delar av dokumentet genom att använda olika metoder, till exempel ```as<int>``` för att hämta ett heltal eller ```as<string>``` för att hämta en sträng.

### Exempel på utmatning
För att visa hur detta fungerar i praktiken, låt oss anta att vi har ett YAML-dokument som innehåller information om en person:
```YAML
name: John
age: 30
```
Genom att använda följande kod:
```C++
#include <iostream>

std::cout << doc["name"].as<string> << std::endl; // Utskrift: John
std::cout << doc["age"].as<int> << std::endl; // Utskrift: 30
```
Vi kan enkelt få tillgång till och skriva ut informationen som lagras i vårt YAML-dokument.

## Djupdykning

Nu när vi har sett hur vi kan använda YAML i en C++ miljö, låt oss titta på några andra aspekter av detta:
- YAML är ett lättläst format vilket gör det enkelt att handskas med även för icke-tekniska användare.
- Det är också ett portabelt format, vilket betyder att det kan användas i olika miljöer utan att behöva göra stora ändringar.
- YAML är också ett flexibelt format som kan anpassas för olika typer av datastrukturer.

## Se även

- [yaml-cpp dokumentation](https://github.com/jbeder/yaml-cpp/wiki)
- [YAML.org](https://yaml.org/)
- [C++ dokumentation](https://en.cppreference.com/w/cpp)

Tack för att du valde att läsa denna bloggpost om att använda YAML i en C++ miljö. Vi hoppas att du har lärt dig något nytt och att det kommer att vara till nytta för ditt framtida programmeringsarbete. Lycka till!