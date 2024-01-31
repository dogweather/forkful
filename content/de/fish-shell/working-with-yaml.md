---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML ist ein einfaches Format zum Speichern konfigurierbarer Daten. Programmierer nutzen es wegen seiner Lesbarkeit und einfachen Bearbeitung in zahlreichen Anwendungen, wie der Konfiguration von Software oder der Definition von Datenstrukturen.

## How to:
Installieren Sie `yaml`, ein Tool für die YAML-Verarbeitung, mit:
```Fish Shell
fisher install gazorby/fish-yaml
```

Lesen eines Wertes:
```Fish Shell
echo "fruits: [apple, banana, cherry]" | yaml read -
```
Ausgabe:
```Fish Shell
- apple
- banana
- cherry
```

Setzen eines Wertes:
```Fish Shell
echo "pets:\n cat: whiskers\n dog: rover" | yaml write - pets.fish goldie
```
Ausgabe:
```Fish Shell
pets:
  cat: whiskers
  dog: rover
  fish: goldie
```

## Deep Dive
YAML, kurz für "YAML Ain't Markup Language", wurde Anfang der 2000er als eine vereinfachte Alternative zu XML entwickelt. Alternativen zu YAML wären z.B. JSON oder TOML, die sich jeweils in ihrer Syntax und dem Anwendungszweck unterscheiden. YAML hebt sich durch seine Stärke bei der Darstellung komplexer Datenstrukturen und der einfachen Les- und Schreibbarkeit für Menschen ab.

## See Also
- YAML-Offizielle Website: https://yaml.org
- YAML-Syntax: https://learnxinyminutes.com/docs/yaml/
- fish-yaml GitHub Repo: https://github.com/gazorby/fish-yaml
