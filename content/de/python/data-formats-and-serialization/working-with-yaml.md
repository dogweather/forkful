---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:28.232352-07:00
description: "YAML, das f\xFCr \"YAML Ain't Markup Language\" steht, ist ein f\xFC\
  r Menschen lesbares Daten-Serialisierungsformat. Programmierer verwenden YAML f\xFC\
  r\u2026"
lastmod: '2024-03-13T22:44:53.398932-06:00'
model: gpt-4-0125-preview
summary: "YAML, das f\xFCr \"YAML Ain't Markup Language\" steht, ist ein f\xFCr Menschen\
  \ lesbares Daten-Serialisierungsformat. Programmierer verwenden YAML f\xFCr\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?
YAML, das für "YAML Ain't Markup Language" steht, ist ein für Menschen lesbares Daten-Serialisierungsformat. Programmierer verwenden YAML für Konfigurationsdateien, interprozessuale Nachrichtenübermittlung und Datenspeicherung aufgrund seiner einfachen Syntax und leichten Lesbarkeit im Vergleich zu anderen Formaten wie XML oder JSON.

## Wie:
Das Lesen und Schreiben von YAML in Python erfordert in der Regel die Verwendung einer Drittanbieter-Bibliothek, wobei `PyYAML` die beliebteste ist. Um zu beginnen, müssen Sie PyYAML installieren, indem Sie `pip install PyYAML` ausführen.

**Beispiel: In eine YAML-Datei schreiben**

```python
import yaml

data = {'a list': [1, 42, 3.141, 1337, 'help', u'€'],
        'a string': 'boo!',
        'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Das erstellt `example.yaml` mit den Daten im YAML-Format strukturiert.
```

**Beispiel: Aus einer YAML-Datei lesen**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# Ausgabe: 
# {'a list': [1, 42, 3.141, 1337, 'help', '€'],
#  'a string': 'boo!',
#  'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}
```

**YAML für Konfiguration verwenden**

Viele Programmierer verwenden YAML zur Verwaltung von Anwendungskonfigurationen. Hier ist ein Beispiel, wie man eine Konfigurationsdatei strukturieren und lesen könnte:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secret
```

Lesen der Konfigurationsdatei in Python:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # Ausgabe: localhost
```

**Umgang mit komplexen Strukturen**

Für komplexe Strukturen ermöglicht PyYAML, benutzerdefinierte Python-Objekte zu definieren. Sorgen Sie jedoch für sichere Praktiken, indem Sie `safe_load` verwenden, um das Ausführen beliebiger Funktionen oder Objekte zu vermeiden.

```python
import yaml

# Ein Python-Objekt definieren
class Example:
    def __init__(self, value):
        self.value = value

# Benutzerdefinierter Konstruktor
def constructor_example(loader, node):
    value = loader.construct_scalar(node)
    return Example(value)

# Konstruktor für das Tag "!example" hinzufügen
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'data'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.value)  # Ausgabe: data
```

In diesem Ausschnitt ist `!example` ein benutzerdefiniertes Tag, das verwendet wird, um ein `Example` Objekt mit dem Wert 'data' aus einem YAML-String zu instanziieren. Benutzerdefinierte Lader wie dieser erweitern die Flexibilität von PyYAML und ermöglichen die Verarbeitung komplexerer Datenstrukturen und Typen.
