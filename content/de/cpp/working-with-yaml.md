---
title:                "C++: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit YAML beschäftigen? YAML ist eine einfache und intuitive Möglichkeit, Daten in einem strukturierten Format zu speichern. Es kann in verschiedenen Programmiersprachen verwendet werden und ist besonders nützlich für die Speicherung von Konfigurationsdateien.

## Wie man es macht
Das Arbeiten mit YAML in C++ ist einfach und unkompliziert. Zunächst müssen wir die Bibliothek "yaml-cpp" installieren, die uns ermöglicht, YAML-Dateien zu lesen und zu schreiben. Schauen wir uns ein Beispiel an:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
  // Eine YAML-Datei wird gelesen
  YAML::Node config = YAML::LoadFile("meineconfig.yaml");

  // Zugriff auf den Wert eines Schlüssels
  std::cout << config["benutzername"].as<std::string>() << "\n";

  // Erstellen einer neuen YAML-Datei
  YAML::Emitter out;
  out << YAML::BeginMap;
  out << YAML::Key << "server";
  out << YAML::Value << "localhost";
  out << YAML::EndMap;

  // Die Datei wird geschrieben
  std::ofstream("neueconfig.yaml") << out.c_str();

  return 0;
}
```

Die Ausgabe für die Beispiel-YAML-Datei "meineconfig.yaml" könnte so aussehen:

```yaml
benutzername: Max
passwort: geheim
server:
  host: example.com
  port: 8080
```

Und dies wäre die Ausgabe für die neu erstellte YAML-Datei "neueconfig.yaml":

```yaml
server: localhost
```

## Tiefer ins Detail gehen
YAML bietet verschiedene Datentypen wie Strings, Zahlen, Arrays und Maps. Auch verschachtelte Strukturen sind möglich. Um beispielsweise auf den Port des Servers in der YAML-Datei "meineconfig.yaml" zuzugreifen, könnten wir folgenden Code verwenden:

```C++
int port = config["server"]["port"].as<int>();
```

Es ist außerdem möglich, benutzerdefinierte Datentypen in YAML zu definieren, indem man sie serialisiert und deserialisiert. YAML-Dateien sind auch gut lesbar für Menschen, was sie zu einer guten Wahl für Konfigurationsdateien macht.

## Siehe auch
- Weitere Informationen über YAML: https://yaml.org/
- Die offizielle Dokumentation von yaml-cpp: https://github.com/jbeder/yaml-cpp/blob/master/doc/README.md