---
title:                "Arbeiten mit yaml"
html_title:           "Ruby: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist eine einfache und lesbarere Art, Daten zu speichern und zu übertragen. Programmer verwenden es, um Code- oder Konfigurationsdateien zu erstellen, da es leicht zu lesen und zu bearbeiten ist.

## Wie?
Um YAML in Ruby zu verwenden, müssen Sie zuerst das YAML-Modul importieren. Dann können Sie JSON-ähnliche Strukturen erstellen und sie mit der Methode `to_yaml` in ein YAML-Format konvertieren. Umgekehrt können Sie mit `YAML.load` YAML-Dateien in ein Ruby-Objekt umwandeln. Hier ist ein Beispiel:
```Ruby
require 'yaml'

# Creating a YAML file
hash = { name: "John", age: 30 }
File.write("person.yml", hash.to_yaml)

# Loading YAML file
person = YAML.load(File.read("person.yml"))
puts person[:name] # Output: John
```

## Tiefer tauchen
YAML wurde ursprünglich vom japanischen Programmierer Ingy döt Net entwickelt und steht für "YAML Ain't Markup Language". Es ist eine Alternative zu anderen Datenformaten wie XML und JSON und verwendet eine einfache Syntax, die auf Einrückungen basiert. YAML ist auch in anderen Programmiersprachen wie Python und Java beliebt. Für weitere Informationen über die YAML-Syntax und -Funktionen können Sie die offizielle Dokumentation konsultieren.

## Siehe auch
- Offizielle YAML-Dokumentation: https://yaml.org/
- Einführung in YAML: https://www.codecademy.com/articles/what-is-yaml