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

## Vad & Varför?

Arbetar du med YAML i dina C++-projekt men undrar vad det är och varför man använder det? YAML är ett språk för att strukturera data i filer, som till exempel konfigurationsdata eller informationsutbyten. Programerare använder YAML för att göra det enklare och mer läsbart att organisera och dela data inom sina program.

## Hur man:

```C++
#include <iostream>
#include <yaml-cpp/yaml.h>

int main() {
  // Skapar en YAML-fil
  YAML::Emitter out;
  out << YAML::BeginMap;
  out << YAML::Key << "Person" << YAML::Value << "John Smith";
  out << YAML::Key << "Age" << YAML::Value << 25;
  out << YAML::EndMap;

  // Skriver ut filen
  std::cout << "YAML-fil:" << std::endl;
  std::cout << out.c_str() << std::endl;

  // Läser av YAML-filen
  YAML::Node node = YAML::Load(out.c_str());
  std::cout << "Data:" << std::endl;
  std::cout << "Namn: " << node["Person"].as<std::string>() << std::endl;
  std::cout << "Ålder: " << node["Age"].as<int>() << std::endl;

  return 0;
}
```

Körning: 

```
YAML-fil:
Person: John Smith
Age: 25

Data:
Namn: John Smith
Ålder: 25
```

## Djupdykning

YAML står för "YAML Ain't Markup Language" och skapades som ett alternativ till XML för att lösa problem med läsbarhet och komplexitet. YAML är utformat för att enkelt kunna läsas av människor och dess syntax är baserad på indentering istället för taggar, vilket gör det lättare att läsa och uppdatera datafiler. YAML används i många olika programmeringsspråk och det finns många alternativ för att arbeta med YAML i C++, som till exempel biblioteket "yaml-cpp" som användes i exemplet ovan. 

## Se även

- [yaml-cpp](https://github.com/jbeder/yaml-cpp)
- [YAML:s officiella hemsida](https://yaml.org/)