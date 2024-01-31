---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "YAML-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
YAML (YAML Ain't Markup Language) on data serialisointiformaatti, johon tallennetaan ja josta luetaan tietoja C++ ohjelmissa. Ohjelmoijat käyttävät YAMLia, koska se on ihmisen luettavissa ja konfiguraatiota on helppo kirjoittaa ja ylläpitää.

## Kuinka:
YAML-käsittely edellyttää kirjastoa. Esimerkiksi `yaml-cpp` on suosittu valinta. 
```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    // YAML-tiedoston luku
    YAML::Node config = YAML::LoadFile("config.yaml");

    // Tietojen haku
    std::string username = config["user"]["name"].as<std::string>();
    int age = config["user"]["age"].as<int>();

    std::cout << "Name: " << username << ", Age: " << age << std::endl;

    // YAML-tiedoston kirjoitus
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "user";
    out << YAML::Value << YAML::BeginMap;
    out << YAML::Key << "name" << YAML::Value << "John Doe";
    out << YAML::Key << "age" << YAML::Value << 30;
    out << YAML::EndMap;
    out << YAML::EndMap;

    std::ofstream fout("new_config.yaml");
    fout << out.c_str();
    
    return 0;
}
```

Oletetaan, että `config.yaml` sisältää:
```yaml
user:
  name: Matti
  age: 25
```

Ohjelman tulostus:
```
Name: Matti, Age: 25
```

`new_config.yaml` tiedostossa on nyt:
```yaml
user:
  name: John Doe
  age: 30
```

## Syväsukellus
YAML kehitettiin 2001 korvaamaan monimutkaisempia valintoja, kuten XML. Sen ihmisen luettavuus ja yksinkertaisuus viehättävät, mutta isojen tietomäärien kanssa YAML ei välttämättä ole tehokkain valinta. Json ja XML ovat suosittuja vaihtoehtoja. `yaml-cpp` kirjaston käyttö C++:ssa tarjoaa luokkia ja funktioita YAML-tiedostojen käsittelyyn. 

## Katso Myös
- yaml-cpp GitHub-sivu: https://github.com/jbeder/yaml-cpp
- YAML virallinen sivusto: https://yaml.org
- YAML standardi: https://yaml.org/spec/1.2/spec.html
- Json ja XML vertailu: https://www.json.org/xml.html
