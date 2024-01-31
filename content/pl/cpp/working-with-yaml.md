---
title:                "Praca z yaml"
date:                  2024-01-19
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z YAML polega na obsłudze danych w formacie, który jest czytelny dla człowieka i maszyny. Programiści używają YAML, bo jest prosty w zapisie i idealny do konfiguracji czy serializacji danych.

## Jak to zrobić:

```C++
#include <iostream>
#include <yaml-cpp/yaml.h>

int main() {
    // Load YAML content from string
    YAML::Node config = YAML::Load("name: Jan\nage: 30\n");

    std::cout << "Name: " << config["name"].as<std::string>() << "\n";
    std::cout << "Age: " << config["age"].as<int>() << std::endl;

    // Create new YAML node and output to string
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "language" << YAML::Value << "C++";
    out << YAML::Key << "version" << YAML::Value << "20";
    out << YAML::EndMap;

    std::cout << "Generated YAML:\n" << out.c_str() << std::endl;

    return 0;
}
```

Sample output:
```
Name: Jan
Age: 30
Generated YAML:
language: C++
version: 20
```

## Wnikliwa analiza:

YAML, czyli "YAML Ain’t Markup Language" (YAML to nie jest język znaczników), to format danych używany od początku lat 2000. Alternatywą dla YAML jest JSON czy XML, jednak YAML często wygrywa czytelnością. Do pracy z YAML w C++ użyjemy biblioteki `yaml-cpp`, która tłumaczy YAML na struktury zrozumiałe w C++. Implementacja YAML w projekcie wymaga zrozumienia struktur, takich jak mapy czy sekwencje, a także aspektów serializacji i deserializacji danych.

## Zobacz również:

- Dokumentacja `yaml-cpp`: [https://github.com/jbeder/yaml-cpp](https://github.com/jbeder/yaml-cpp)
- Specyfikacja YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Porównanie formatów danych (JSON vs. YAML): [https://www.json2yaml.com/](https://www.json2yaml.com/)
