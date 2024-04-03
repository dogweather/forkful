---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:58.687308-07:00
description: 'Hoe: YAML-ondersteuning is niet ingebouwd in C++. Je hebt een bibliotheek
  zoals `yaml-cpp` nodig. Hier is hoe je een eenvoudig YAML-bestand parseert.'
lastmod: '2024-03-13T22:44:51.133619-06:00'
model: gpt-4-0125-preview
summary: YAML-ondersteuning is niet ingebouwd in C++.
title: Werken met YAML
weight: 41
---

## Hoe:
YAML-ondersteuning is niet ingebouwd in C++. Je hebt een bibliotheek zoals `yaml-cpp` nodig. Hier is hoe je een eenvoudig YAML-bestand parseert:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("config.yaml");
    YAML::Node config = YAML::Load(file);
    
    std::string username = config["user"]["name"].as<std::string>();
    int age = config["user"]["age"].as<int>();
    
    std::cout << "Naam: " << username << ", Leeftijd: " << age << std::endl;
    return 0;
}
```

Uitgaande van dat `config.yaml` is:
```
user:
  name: John Doe
  age: 30
```

Uitvoer:
```
Naam: John Doe, Leeftijd: 30
```

## Diepe Duik
YAML werd voor het eerst geïntroduceerd in 2001 als een voor mensen leesbare gegevensserialisatienorm. Terwijl JSON en XML veelvoorkomende alternatieven zijn, heeft de minimale syntaxis van YAML het populair gemaakt voor configuratiebestanden. Bibliotheken zoals `yaml-cpp` behandelen het parsen en uitstoten van YAML-gegevens, en vertegenwoordigen deze in structuren zoals mappen en sequenties, vergelijkbaar met JSON-objecten en -arrays.

## Zie Ook
- YAML 1.2 Specificatie: https://yaml.org/spec/1.2/spec.html
- yaml-cpp GitHub Repository: https://github.com/jbeder/yaml-cpp
- Een Introductie tot YAML: https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started
