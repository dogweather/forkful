---
title:                "Arbeid med yaml"
html_title:           "C++: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en C++ programmerer, har du sannsynligvis hørt om YAML-filer. Som er enkelt og leselig format som brukes for å lagre og overføre data. Denne artikkelen vil fortelle deg hvorfor det er verdt å lære hvordan du kan jobbe med YAML i din C++ kode.

## Hvordan

For å jobbe med YAML i C++, må du først inkludere yaml-cpp biblioteket i din kode. Dette biblioteket tillater deg å lese, skrive og manipulere YAML-filer i din C++ applikasjon.

```C++
#include <yaml-cpp/yaml.h>
```

Deretter kan du enkelt lese en YAML-fil ved å bruke YAML::Load() metoden og angi filnavnet som parameter. Dette vil returnere en YAML::Node som kan brukes til å få tilgang til dataene i filen.

```C++
YAML::Node data = YAML::Load("example.yaml");
```

For å få tilgang til en bestemt dataen i noden, kan du bruke operator[] og angi nøkkelen til dataen som du ønsker å hente.

```C++
std::string name = data["name"].as<std::string>();
```

Du kan også enkelt legge til data i en YAML-fil ved å bruke operator<< og angi nøkkelen og verdien til dataen du ønsker å legge til.

```C++
data << "age" << 25;
```

For å skrive endringene tilbake til filen, kan du bruke YAML::Emitter og YAML::Node::Write() metoden.

```C++
YAML::Emitter out;
out << data;
std::ofstream myfile("example.yaml");
myfile << out.c_str();
```

## Deep Dive

En av fordelene med å jobbe med YAML-filer i C++ er at det er enkelt og effektivt å lese og skrive store mengder data. YAML støtter alle de grunnleggende datatypene, inkludert int, string, float og bool. Det støtter til og med lister og komplekse datatyper som kan være nyttige hvis du trenger å lagre hierarkisk data.

YAML::Node har også mange nyttige metoder som YAML::Node::size() for å få størrelsen på en liste, YAML::Node::begin() og YAML::Node::end() for å iterere gjennom en liste, og YAML::Node::IsScalar() for å sjekke om noden inneholder en enkel verdi.

Hvis du vil ha en mer detaljert forklaring på hvordan du kan jobbe med YAML i C++, kan du sjekke ut yaml-cpp dokumentasjonen på GitHub (https://github.com/jbeder/yaml-cpp/wiki).

## Se også

- YAML.org (https://yaml.org/)
- yaml-cpp dokumentasjon (https://github.com/jbeder/yaml-cpp/wiki)