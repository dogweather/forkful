---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML, "YAML Ain't Markup Language", ist ein Format zum Speichern und Übertragen von Daten, das lesbar und leicht bearbeitbar ist. Programmierer nutzen YAML wegen seiner Simplizität und Menschenlesbarkeit, oft für Konfigurationsdateien und Dateninterchange.

## How to:
Installation des PyYAML-Pakets:
```Python
pip install PyYAML
```

Lesen einer YAML-Datei:
```Python
import yaml

with open('config.yaml', 'r') as file:
    config = yaml.safe_load(file)

print(config)
```

Ergebnis:
```
{'Einstellungen': {'Auflösung': '1920x1080', 'Lautstärke': 75}}
```

Schreiben in eine YAML-Datei:
```Python
data = {'Name': 'Max', 'Alter': 30, 'Beruf': 'Ingenieur'}

with open('user.yaml', 'w') as file:
    yaml.dump(data, file)
```

## Deep Dive
YAML entstand Anfang der 2000er als eine einfachere Alternative zu XML und JSON. Es verwendet Einrückungen zur Darstellung von Hierarchien, was die Lesbarkeit verbessert. Alternativen zu YAML sind JSON und TOML; JSON ist weniger leserlich aber schneller, und TOML fokussiert ebenfalls auf Lesbarkeit. Besonders die PyYAML-Implementation hat sich wegen ihrer Kompatibilität und Vielseitigkeit durchgesetzt.

## See Also
- YAML offizielle Seite: [https://yaml.org/](https://yaml.org/)
- PyYAML Dokumentation: [https://pyyaml.org/wiki/PyYAMLDocumentation](https://pyyaml.org/wiki/PyYAMLDocumentation)
- JSON Webseite: [https://www.json.org/json-de.html](https://www.json.org/json-de.html)
- TOML GitHub Repo: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
