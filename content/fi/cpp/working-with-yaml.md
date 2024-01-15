---
title:                "Työskentely yaml-koodin kanssa"
html_title:           "C++: Työskentely yaml-koodin kanssa"
simple_title:         "Työskentely yaml-koodin kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on yhä suositumpi tiedostomuoto ohjelmointimaailmassa, erityisesti projektinhallinnassa, konfiguraatiotiedostoissa ja tiedonsiirtomuodona. Sen yksinkertaisuus ja helppokäyttöisyys tekevät siitä houkuttelevan vaihtoehdon muihin tiedostomuotoihin nähden. 

## Kuinka

```C++
#include <iostream>
#include <yaml-cpp/yaml.h>

int main() {
  // Luodaan YAML-tiedosto
  YAML::Emitter emitter;
  emitter << YAML::BeginMap;
  emitter << YAML::Key << "nimi";
  emitter << YAML::Value << "Jane";
  emitter << YAML::Key << "ikä";
  emitter << YAML::Value << 25;
  emitter << YAML::EndMap;

  // Talletetaan tiedosto
  std::ofstream file("tiedosto.yaml");
  file << emitter.c_str();
  file.close();

  // Luetaan YAML-tiedostosta
  YAML::Node data = YAML::LoadFile("tiedosto.yaml");

  // Tulostetaan YAML-data
  std::cout << "Nimi: " << data["nimi"].as<std::string>() << std::endl;
  std::cout << "Ikä: " << data["ikä"].as<int>() << std::endl;
  
  return 0;
}
```
### Tulostus:
```
Nimi: Jane
Ikä: 25
```

## Syväsukellus

YAML on helppo lukea ja ymmärtää sekä ihmiselle että ohjelmalle, ja sen syntaxi on hyvin joustava. YAML-tiedosto koostuu avain-arvo -pareista ja sisentämisellä on merkitystä tiedoston rakenteen kannalta. Tästä johtuen YAML antaa mahdollisuuden luoda selkeitä ja helposti ylläpidettäviä tiedostoja. 

Tiedostossa on myös mahdollista käyttää erilaisia datatyyppejä kuten numeroita, merkkijonoja, luetteloita ja karttoja. Lisäksi YAML tukee kommentteja, joita voi käyttää selittämään tiedoston sisältöä. 

## Katso myös

- [YAML-käsikirja](https://yaml.org/)
- [YAML-CPP-kirjaston dokumentaatio](https://github.com/jbeder/yaml-cpp/wiki)
- [YAML:n käyttö C++:ssa -opas](https://www.geeksforgeeks.org/yaml-cpp-library-c-stl-like-interface-yaml/)