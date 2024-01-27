---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein Datenformat für Konfigurationsdateien, das Lesbarkeit betont. Programmierer nutzen es, weil es menschenfreundlicher als XML oder JSON ist und um Konfigurationen für vielfältige Anwendungen, wie Kubernetes oder Docker, zu definieren.

## How to:
Arbeiten mit YAML in Bash kann mithilfe von `yq` erfolgen, einem kommandozeilenbasierten YAML-Prozessor:

Installation von `yq`:
```Bash
sudo wget https://github.com/mikefarah/yq/releases/download/v4.6.1/yq_linux_amd64 -O /usr/bin/yq && sudo chmod +x /usr/bin/yq
```

Lesen eines Wertes:
```Bash
cat config.yaml | yq e '.server.port'
# Ausgabe: 8080
```

Ändern eines Wertes:
```Bash
yq e '.server.port = "9090"' -i config.yaml
```

Überprüfen der Änderung:
```Bash
yq e '.server.port' config.yaml
# Ausgabe: 9090
```

## Deep Dive
YAML wurde erstmals Anfang der 2000er als eine einfachere Alternative zu XML eingeführt. Alternativen zu YAML sind JSON und TOML – jeweils mit eigenen Stärken. YAML nutzt Einrückungen zur Strukturierung, was Fehler anfällig machen kann. Tools wie `yq` basieren intern auf LibYAML für Parsing und Serialisierung und sorgen für effizientes Arbeiten mit YAML-Dateien.

## See Also
- YAML Spezifikation: https://yaml.org/spec/1.2/spec.html
- `yq` GitHub Repository: https://github.com/mikefarah/yq
- YAML und JSON vergleich: https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json
- Einführung in YAML und seine Unterschiede zu JSON: https://www.redhat.com/de/topics/automation/what-is-yaml
