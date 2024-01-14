---
title:                "Python: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit YAML beschäftigen? YAML (YAML Ain't Markup Language) ist ein häufig verwendetes Format für die Speicherung und Übertragung von Daten. Es ist besonders nützlich für die Konfiguration von Software und ermöglicht es, Daten in einem einfachen, menschenlesbaren Format zu speichern.

## Wie

Um mit YAML in Python zu arbeiten, muss man zuerst das "pyyaml" Paket importieren. Dann kann man mit der Funktion "safe_load()" eine YAML-Datei in ein Python Dictionary umwandeln:

```Python
import yaml

with open("beispiel.yaml") as f:
    data = yaml.safe_load(f)

print(data)
```
Die Ausgabe sieht dann wie folgt aus:

```Python
{'name': 'John', 'age': 30, 'hobbies': ['hiking', 'cooking', 'reading']}
```

Man kann nun auf die einzelnen Einträge im Dictionary zugreifen, z.B. um den Namen auszugeben:

```Python
print("Name: {}".format(data['name']))
```
Die Ausgabe wäre dann: "Name: John".

## Tiefer Einblick

YAML bietet noch weitere nützliche Funktionen, wie z.B. die Möglichkeit, Variablen und Listen einzubinden oder komplexe Datenstrukturen wie verschachtelte Dictionaries zu erstellen. Es ist auch möglich, YAML-Dokumente zu validieren und Fehlermeldungen auszugeben, um mögliche Probleme bei der Verarbeitung zu erkennen.

Es ist jedoch wichtig zu beachten, dass YAML auf Einrückungen und Leerzeichen angewiesen ist, um die Struktur der Daten zu definieren. Damit kann es manchmal zu unerwarteten Fehlern führen, wenn die Formatierung nicht korrekt ist.

## Siehe auch

Weitere Informationen und Beispiele für die Arbeit mit YAML in Python finden Sie unter folgenden Links:

- [Offizielle Dokumentation für pyyaml](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Beispiele für die Arbeit mit YAML in Python](https://realpython.com/python-yaml/)
- [Tutorial für YAML in Python](https://zetcode.com/python/yaml/)

Vielen Dank fürs Lesen und viel Spaß beim Programmieren mit YAML!