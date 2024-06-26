---
date: 2024-01-19
description: 'How to: Per iniziare, installa la libreria `yaml-cpp` usando il gestore
  di pacchetti che preferisci. Ecco un esempio di lettura e scrittura.'
lastmod: '2024-03-13T22:44:43.748497-06:00'
model: unknown
summary: Per iniziare, installa la libreria `yaml-cpp` usando il gestore di pacchetti
  che preferisci.
title: Lavorare con YAML
weight: 41
---

## How to:
Per iniziare, installa la libreria `yaml-cpp` usando il gestore di pacchetti che preferisci. Ecco un esempio di lettura e scrittura:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

// Scrivere YAML in un file
void writeYAML() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "nome" << YAML::Value << "Mario Rossi";
    out << YAML::Key << "età" << YAML::Value << 30;
    out << YAML::EndMap;

    std::ofstream fout("persona.yaml");
    fout << out.c_str();
}

// Leggere YAML da un file
void readYAML() {
    std::ifstream fin("persona.yaml");
    YAML::Node config = YAML::Load(fin);
    std::cout << "Nome: " << config["nome"].as<std::string>() << std::endl;
    std::cout << "Età: " << config["età"].as<int>() << std::endl;
}

int main() {
    writeYAML();
    readYAML();
    return 0;
}
```

Output:
```
Nome: Mario Rossi
Età: 30
```

## Deep Dive
YAML, che sta per "YAML Ain't Markup Language", è nato nel 2001 per essere un superset leggibile di JSON. Alternative popolari a YAML sono JSON per la compatibilità con il web e TOML per la semplicità. YAML usa un modello a grafo per rappresentare strutture dati, che consente riferimenti ciclici e il riuso di nodi, mentre JSON si limita ad una struttura ad albero.

## See Also
- yaml-cpp GitHub: https://github.com/jbeder/yaml-cpp
- Documentazione YAML ufficiale: https://yaml.org/spec/1.2/spec.html
- Confronto tra YAML e JSON: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
