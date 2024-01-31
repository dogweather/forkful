---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
YAML ist ein menschenlesbares Datenformat. Entwickler nutzen es für Konfigurationsdateien und Datenübertragungen, weil es einfach und klar ist.

## How to: (Wie geht das?)
C++ hat keine eingebaute YAML-Unterstützung, daher verwenden wir die `yaml-cpp`-Bibliothek. Hier installierst du sie und liest eine YAML-Datei:

```cpp
// yaml-cpp installieren: 
// Folge den Anweisungen auf https://github.com/jbeder/yaml-cpp

#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    // YAML-Datei laden
    YAML::Node config = YAML::LoadFile("config.yaml");

    // Zugriff auf Werte
    std::string host = config["host"].as<std::string>();
    int port = config["port"].as<int>();

    // Ausgabe
    std::cout << "Host: " << host << "\n";
    std::cout << "Port: " << port << "\n";

    return 0;
}
```

Beispiel `config.yaml`:
```yaml
host: localhost
port: 8080
```

Sample Output:
```
Host: localhost
Port: 8080
```

## Deep Dive (Tiefergehendes)
YAML entstand Anfang der 2000er als Alternative zu XML und ähnelt JSON. Im Gegensatz zu JSON betont YAML Lesbarkeit und unterstützt Kommentare. `yaml-cpp` bietet eine C++-spezifische Implementierung, aber es gibt auch Bibliotheken für andere Sprachen wie PyYAML für Python.

## See Also (Siehe auch)
- YAML-Spezifikation: https://yaml.org/spec/1.2/spec.html
- `yaml-cpp` GitHub-Repo: https://github.com/jbeder/yaml-cpp
- YAML-Tutorial: https://learnxinyminutes.com/docs/yaml/
