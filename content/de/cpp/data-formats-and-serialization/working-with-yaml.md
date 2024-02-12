---
title:                "Arbeiten mit YAML"
aliases:
- /de/cpp/working-with-yaml/
date:                  2024-02-03T19:24:32.112091-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

YAML, das für "YAML Ain't Markup Language" steht, ist ein für Menschen lesbaren Daten-Serialisierungsformat. Programmierer nutzen es für Konfigurationsdateien, Datendumping und zum Speichern von hierarchischen Daten wegen seiner Lesbarkeit und einfach zu verstehenden Syntax im Vergleich zu XML oder JSON.

## Wie:

Um mit YAML in C++ zu arbeiten, ist eine beliebte Wahl die Bibliothek `yaml-cpp`. Stellen Sie zunächst sicher, dass Sie `yaml-cpp` installiert und richtig mit Ihrem C++-Projekt verlinkt haben.

**Ein YAML-Datei lesen:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Titel: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Angenommen, eine `config.yaml` sieht so aus:

```yaml
title: "Beispiel YAML"
```

Die Ausführung des obenstehenden C++-Codes würde produzieren:

```
Titel: Beispiel YAML
```

**In eine YAML-Datei schreiben:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Beispiel YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

Dieser Code wird eine `output.yaml` mit dem Inhalt erstellen:

```yaml
title: Beispiel YAML
```

Diese Beispiele dienen als eine grundlegende Einführung zum Lesen von und Schreiben in YAML-Dateien in C++ unter Verwendung der `yaml-cpp` Bibliothek. Für komplexere Strukturen und Anwendungsfälle erkunden Sie die `yaml-cpp` Dokumentation für Funktionen wie Sequenzen, Tags und fortgeschrittenere Serialisierungs- und Deserialisierungstechniken.
