---
title:                "Arbeiten mit YAML"
html_title:           "Python: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?

YAML ist eine einfache, menschenlesbare Datennotation, die oft von Programmierern als Alternative zu anderen Datenformaten verwendet wird. Es ist besonders nützlich für die Konfiguration von Anwendungen und das Speichern von Daten, die in menschenlesbarer Form bleiben sollen.

## Wie geht's:

Im Folgenden werden einige Beispiele für die Verwendung von YAML in Python gezeigt:

### Lesen einer YAML-Datei:

```Python
import yaml

# Lese die Datei "config.yml" ein
with open("config.yml", "r") as file:
    # Nutze die load-Methode des yaml-Moduls, um die Daten zu laden
    data = yaml.load(file)

# Gib die geladenen Daten aus
print(data)
```

### Schreiben einer YAML-Datei:

```Python
import yaml

# Definiere die Daten, die gespeichert werden sollen
data = {"Name": "Max", "Alter": 25, "Lieblingsessen": "Pizza"}

# Nutze die dump-Methode des yaml-Moduls, um die Daten in eine Datei zu schreiben
with open("daten.yml", "w") as file:
    yaml.dump(data, file)
```

### Ausgabe:

```Python
{'Name': 'Max', 'Alter': 25, 'Lieblingsessen': 'Pizza'}
```

## Tiefere Einblicke:

### Historischer Hintergrund:

YAML wurde ursprünglich entwickelt, um eine einfache Alternative zu XML zu bieten. Es ist jedoch noch nicht so weit verbreitet wie JSON. Aufgrund seiner Einfachheit und Lesbarkeit wird es jedoch zunehmend von Programmierern bevorzugt.

### Alternativen:

Alternativen zu YAML in Python sind unter anderem das JSON-Modul und die configparser-Bibliothek. Während YAML einerseits auf Lesbarkeit und Flexibilität ausgerichtet ist, zielen diese Alternativen eher auf Struktur und Geschwindigkeit ab.

### Implementierungsdetails:

Das yaml-Modul in Python basiert auf der Bibliothek PyYAML, die in C geschrieben ist. Daher ist es im Allgemeinen schneller als reine Python-Implementierungen von YAML.

## Sieh auch:

Weitere Informationen zu YAML in Python findet ihr in der offiziellen Dokumentation unter https://pyyaml.org/wiki/PyYAMLDocumentation (Englisch). Eine Liste von Alternativen zu YAML ist unter https://www.fullstackpython.com/yaml.html zu finden.