---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:58.687308-07:00
description: "Werken met YAML houdt in dat je gegevens parseert en genereert in de\
  \ voor mensen vriendelijke YAML Ain't Markup Language. Programmeurs gebruiken het\
  \ voor\u2026"
lastmod: 2024-02-19 22:05:10.220703
model: gpt-4-0125-preview
summary: "Werken met YAML houdt in dat je gegevens parseert en genereert in de voor\
  \ mensen vriendelijke YAML Ain't Markup Language. Programmeurs gebruiken het voor\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met YAML houdt in dat je gegevens parseert en genereert in de voor mensen vriendelijke YAML Ain't Markup Language. Programmeurs gebruiken het voor configuratiebestanden, gegevensserialisatie en toepassingsinstellingen vanwege de leesbaarheid en eenvoud.

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
