---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:32.112091-07:00
description: "YAML, das f\xFCr \"YAML Ain't Markup Language\" steht, ist ein f\xFC\
  r Menschen lesbaren Daten-Serialisierungsformat. Programmierer nutzen es f\xFCr\u2026"
lastmod: '2024-03-13T22:44:54.204676-06:00'
model: gpt-4-0125-preview
summary: "YAML, das f\xFCr \"YAML Ain't Markup Language\" steht, ist ein f\xFCr Menschen\
  \ lesbaren Daten-Serialisierungsformat."
title: Arbeiten mit YAML
weight: 41
---

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
