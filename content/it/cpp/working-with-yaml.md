---
title:                "Lavorare con yaml"
html_title:           "C++: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui lavorare con YAML nella programmazione, ma la più importante è che questo formato di dati è leggibile per gli umani e facile da interpretare per le macchine. Questo lo rende un'ottima scelta per l'archiviazione e lo scambio di dati strutturati tra diversi sistemi.

## Come fare

Per utilizzare YAML nel tuo codice C++, basta includere la libreria "yaml-cpp", disponibile su GitHub. Ci sono molte risorse online per la documentazione e gli esempi di utilizzo di questa libreria.

Ecco un semplice esempio di codice C++ che carica e stampa un file YAML:

```
#include <iostream>
#include <yaml-cpp/yaml.h>
 
int main() {
  YAML::Node config = YAML::LoadFile("config.yml");

  std::cout << "Nome utente: " << config["username"].as<std::string>() << "\n";
  std::cout << "Età: " << config["age"].as<int>() << " anni\n";
  std::cout << "Linguaggi di programmazione preferiti: \n";
  for (auto lang : config["languages"]) {
    std::cout << "- " << lang << "\n";
  }
}
```

Ecco il contenuto del file YAML "config.yml" usato nel codice sopra:

```
username: Alice
age: 25
languages:
  - C++
  - Python
  - Java
```

E questo è l'output che verrà stampato sul terminale:

```
Nome utente: Alice
Età: 25 anni
Linguaggi di programmazione preferiti:
- C++
- Python
- Java
```

## Approfondimento

Se vuoi saperne di più su YAML, è possibile consultare la loro documentazione ufficiale o documentazioni gratuite su siti come YAML.org. Inoltre, ci sono molti tutorial e guide online su come utilizzare YAML in diversi linguaggi di programmazione, compreso C++.

## Vedi anche

- [Documentazione ufficiale di YAML](https://yaml.org)
- [Documentazione di yaml-cpp su GitHub](https://github.com/jbeder/yaml-cpp/wiki)