---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et format for data-seriellisering, tilgjengelig for mennesker, ofte brukt for konfigurasjonsfiler. Programmerere bruker YAML fordi det er lett å lese og støtter komplekse datastrukturer.

## Hvordan gjøre det:
For å jobbe med YAML i C++, trenger du et bibliotek som `yaml-cpp`. Her ser du hvordan lese og skrive:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>

int main() {
    // Skrive til YAML
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "name" << YAML::Value << "Ola Nordmann";
    out << YAML::Key << "age" << YAML::Value << 30;
    out << YAML::EndMap;

    std::ofstream fout("example.yaml");
    fout << out.c_str();
    fout.close();

    // Lese fra YAML
    YAML::Node config = YAML::LoadFile("example.yaml");
    std::cout << "Navn: " << config["name"].as<std::string>() << "\n";
    std::cout << "Alder: " << config["age"].as<int>() << std::endl;
    return 0;
}
```

Kjører du koden, får du:
```
Navn: Ola Nordmann
Alder: 30
```
Og `example.yaml` ser slik ut:
```yaml
name: Ola Nordmann
age: 30
```

## Dypdykk
YAML ("YAML Ain't Markup Language") oppstod på 2000-tallet som et mer lesevennlig alternativ til XML. Andre alternativer inkluderer JSON og TOML. YAML-tolkere, som `yaml-cpp`, tolker datastrukturene til C++ typer. Brukt riktig, er det både kraftig og fleksibelt for appkonfigurasjon og datautveksling.

## Se også:
- yaml-cpp GitHub: https://github.com/jbeder/yaml-cpp
- YAML offisielle side: https://yaml.org/
- YAML spesifikasjon: https://yaml.org/spec/1.2/spec.html
- Tutorial for yaml-cpp: https://github.com/jbeder/yaml-cpp/wiki/Tutorial
