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

## Che cos'è e perché?

Lavorare con YAML significa utilizzare un formato di dati testuale che si basa su coppie chiave-valore. Questo formato è utilizzato principalmente dai programmatori per organizzare e strutturare i dati in maniera leggibile e facilmente manipolabile.

## Come fare:

Utilizzando lo standard di librerie C++ "yaml-cpp", è possibile importare ed esportare facilmente dati in formato YAML attraverso il codice. Di seguito, un esempio di codice che mostra come creare un file YAML e come leggerlo:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

void createYAMLFile() 
{
    YAML::Emitter out;

    out << YAML::BeginMap; // inizio del file YAML
    out << YAML::Key << "name" << YAML::Value << "Mario";
    out << YAML::Key << "age" << YAML::Value << 30;
 
    // creazione di una mappa all'interno del file
    out << YAML::Key << "details";
    out << YAML::BeginMap;
    out << YAML::Key << "height" << YAML::Value << 1.8;
    out << YAML::Key << "weight" << YAML::Value << 80;
    out << YAML::EndMap; // fine della mappa
    out << YAML::EndMap; // fine del file YAML
 
    std::ofstream file("person.yaml");
    file << out.c_str(); // scrittura del file
    file.close();
}

void readYAMLFile() 
{
    YAML::Node data = YAML::LoadFile("person.yaml"); // carica il file YAML
 
    // accesso ai dati all'interno del file
    std::cout << "name: " << data["name"].as<std::string>() << "\n";
    std::cout << "age: " << data["age"].as<int>() << "\n";
    std::cout << "details:\n";
    std::cout << "    height: " << data["details"]["height"].as<float>() << "\n";
    std::cout << "    weight: " << data["details"]["weight"].as<float>() << "\n";
}

int main()
{
    createYAMLFile(); // chiamata alla funzione per creare il file YAML
    readYAMLFile(); // chiamata alla funzione per leggere il file YAML
    return 0;
}
```

Il file YAML creato avrà la seguente struttura:

```yaml
name: Mario
age: 30
details:
    height: 1.8
    weight: 80
```

## Deep Dive:

● YAML è un acronimo ricorsivo che significa "YAML Ain't Markup Language".

● YAML è stato creato da Clark Evans, dopo essersi reso conto che YAML era necessario per Age, un progetto open source.

● I principali formati alternativi a YAML includono JSON, XML e CSV. Mentre JSON è più popolare per l'interscambio dei dati tra i dispositivi, YAML è generalmente utilizzato per la configurazione e la documentazione dei dati.

● Il parser di YAML utilizzato all'interno di "yaml-cpp" è basato sulla specifica YAML 1.2 e supporta una vasta gamma di caratteristiche come tipi personalizzati, referenze, inclusioni e altro.

## See Also:

● Sito ufficiale di YAML: https://yaml.org/

● Documentazione di "yaml-cpp": https://github.com/jbeder/yaml-cpp/wiki