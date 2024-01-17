---
title:                "Arbeiten mit YAML"
html_title:           "Gleam: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?

YAML ist eine Dateiendung, die hauptsächlich in der Programmierung verwendet wird. Sie steht für "Yet Another Markup Language" und dient zur Strukturierung von Daten in einem menschenlesbaren Format. Programmierer nutzen YAML, um ihre Daten zu organisieren und zu speichern.

## Wie geht's?

Hier ist ein Beispiel, wie man in Gleam eine YAML-Datei schreibt:

```Gleam
import gleam/encode/yaml as yaml

let person = {name: "Anna", age: 25}
let yaml_string = yaml.encode(person)

{
    assert yaml_string == "---\name: "Anna"\nage: 25"
    yaml_string
}
```

Das resultierende Format sieht so aus:

```
---
name: "Anna"
age: 25
```

## Tiefer Eintauchen

YAML ist seit 2001 ein offener Standard und wurde entwickelt, um eine einfachere Alternative zu JSON zu bieten. Es wird häufig in Anwendungen verwendet, die viele Konfigurationsdateien haben, wie zum Beispiel Anwendungen auf der Basis von Microservices. Eine Alternative zu YAML ist TOML, das sich jedoch noch nicht so weit verbreitet hat.

Die Implementierung von YAML in Gleam basiert auf der Bibliothek "js-yaml". Sie bietet eine schnelle und zuverlässige Möglichkeit, YAML in Gleam zu nutzen.

## Siehe auch

- [Gleam Dokumentation](https://gleam.run/documentation/)
- [The Basics of YAML](https://www.codeproject.com/Articles/1214409/The-Basics-of-YAML)
- [TOML vs. YAML](https://www.toptal.com/developers/toml/toml-vs-yaml)