---
title:                "Elixir: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum YAML in Elixir verwenden?

YAML ist eine einfache und leistungsstarke Methode, um Daten darzustellen und zu speichern. Es ist besonders nützlich in der Programmierung, da es menschenlesbar ist und eine benutzerfreundliche Alternative zu JSON und XML bietet. Es wird auch häufig in Elixir Projekten verwendet, da es gut mit der Syntax von Elixir harmoniert und eine einfache Integration ermöglicht.

## Wie man YAML in Elixir nutzt

Um YAML in Elixir zu verwenden, muss zuerst das Paket "YamlElixir" installiert werden. Dann kann man es in seinem Code importieren und mit dem beliebten "YAML.load_file/1" function die Daten aus einer YAML-Datei laden. Die Syntax sieht folgendermaßen aus:

```Elixir
import YamlElixir

data = YAML.load_file("data.yml")
```

Eine YAML-Datei könnte beispielsweise folgendermaßen aussehen:

```YAML
name: Jane
age: 28
hobbies:
  - hiking
  - cooking
  - dancing
```

Wenn man nun die Variable "data" ausgibt, würde man folgendes Ergebnis sehen:

```Elixir
%{age: 28, hobbies: ["hiking", "cooking", "dancing"], name: "Jane"}
```

Man kann auch direkt einen String benutzen, um YAML Daten zu laden:

```Elixir
yaml_string = """
name: John
age: 35
hobbies:
  - painting
  - reading
  - gardening
"""

data = YAML.load(yaml_string)
```

Dies würde wiederum folgendes Ergebnis liefern:

```Elixir
%{age: 35, hobbies: ["painting", "reading", "gardening"], name: "John"}
```

## Tiefere Einblicke in die Arbeit mit YAML

YAML bietet eine breite Palette an Funktionen und Möglichkeiten, um Daten zu repräsentieren. Eine wichtige Sache zu beachten, ist die Verwendung von Leerzeichen und Einrückungen, um die Struktur der Daten zu definieren. Diese Leerzeichen und Einrückungen müssen korrekt sein, um fehlerhafte Ergebnisse zu vermeiden.

Zudem können auch komplexe Datenstrukturen wie Listen, Arrays und assoziative Arrays in YAML dargestellt werden. Hier ein Beispiel, wie man in YAML Objekte und deren Eigenschaften miteinander verbinden kann:

```YAML
object:
  name: Jane
  age: 28
  hobbies:
    - hiking
    - cooking
    - dancing
```

Man kann auch Kommentare in YAML einfügen, um den Code besser zu strukturieren und zu verstehen.

## Siehe auch

- Offizielle Elixir Dokumentation zu YAML: https://hexdocs.pm/elixir/YAML.Elixir.html
- YAML Syntax Erklärung: https://yaml.org/spec/1.2/spec.html
- Einführung in Elixir: https://elixir-lang.org/getting-started/introduction.html