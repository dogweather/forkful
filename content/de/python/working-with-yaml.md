---
title:                "Arbeiten mit yaml"
html_title:           "Python: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

YAML ist eine simple und benutzerfreundliche Möglichkeit, Daten in einem menschenlesbaren Format zu speichern und zu übertragen. Es ist besonders nützlich für die Konfiguration von Software oder das Erstellen von Strukturen für einfache Datenaustauschformen.

## Wie geht's

YAML-Dateien können einfach als Textdateien erstellt und bearbeitet werden. Um YAML-Daten in Python einzulesen, muss die `yaml` Bibliothek installiert werden. Danach kann sie mit `import yaml` importiert werden.

```Python
import yaml

# YAML-Datei einlesen
with open("beispiel.yml", "r") as file:
    data = yaml.load(file, Loader=yaml.FullLoader)

# Datenausgabe
print(data)
```

Die Ausgabe könnte folgendermaßen aussehen:

```Python
{'name': 'Max Mustermann', 'alter': 30, 'hobbies': ['Lesen', 'Kochen', 'Sport']}
```

Um YAML-Daten in Python zu schreiben, können die Daten einfach in ein entsprechendes Format gebracht und in eine Datei geschrieben werden.

```Python
import yaml

# Daten zum Schreiben
data = {
    "name": "Max Mustermann",
    "alter": 30,
    "hobbies": ["Lesen", "Kochen", "Sport"]
}

# Daten in YAML-Format bringen
yaml_data = yaml.dump(data)

# YAML-Datei schreiben
with open("beispiel.yml", "w") as file:
    file.write(yaml_data)
```

## Tiefergehende Informationen

YAML steht für "YAML Ain't Markup Language" und wurde entwickelt, um eine einfachere und lesbarere Alternative zu anderen Datenformaten wie XML oder JSON zu bieten. Es basiert auf Einrückungen und Zeilenumbrüchen, um Daten zu strukturieren. YAML wird in vielen Bereichen eingesetzt, von der Webentwicklung bis hin zu Automatisierungstools wie Ansible.

In Python gibt es mehrere Bibliotheken für die Arbeit mit YAML-Daten, darunter `PyYAML` und `yaml-include`. Diese bieten erweiterte Funktionen wie die Unterstützung für Mehrzeilen- und mehrdimensionale Datenstrukturen.

## Siehe auch

- [YAML.org](https://yaml.org/)
- [Offizielle Dokumentation von PyYAML](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [yaml-include Dokumentation](https://pypi.org/project/yaml-include/)