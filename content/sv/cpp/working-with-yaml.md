---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett format för datautbyte lätt att läsa för människor. Programmerare använder det för konfigurationer och data serialization eftersom det är enkelt och tydligt.

## Hur gör man:
```cpp
#include <iostream>
#include <yaml-cpp/yaml.h>

int main() {
    // Parse YAML string
    YAML::Node config = YAML::Load("name: Programmer\nage: 30\nlanguage: C++");

    // Access elements
    std::cout << "Name: " << config["name"].as<std::string>() << "\n";
    std::cout << "Age: " << config["age"].as<int>() << "\n";
    std::cout << "Language: " << config["language"].as<std::string>() << "\n";

    // Create YAML and print
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "framework" << YAML::Value << "Qt";
    out << YAML::Key << "OS" << YAML::Value << "Windows";
    out << YAML::EndMap;
    
    std::cout << out.c_str() << std::endl;
    
    return 0;
}
```
Sample output:
```
Name: Programmer
Age: 30
Language: C++
framework: Qt
OS: Windows
```

## Djupdykning
YAML (YAML Ain't Markup Language) skapades tidigt 2000-tal som ett enklare alternativ till XML. Andra alternativ som JSON och TOML används också för liknande syften, men YAML är ofta föredraget för dess läsbarhet. I C++ hanteras YAML genom bibliotek som `yaml-cpp`, vilket tillhandahåller funktioner för att både läsa från och skriva till YAML-filer.

## Se också
- Officiell `yaml-cpp` GitHub-repositorium: https://github.com/jbeder/yaml-cpp
- YAML officiell webbplats: https://yaml.org
- YAML specifikation: https://yaml.org/spec/1.2/spec.html
- Jämförelse mellan YAML och JSON: https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json
