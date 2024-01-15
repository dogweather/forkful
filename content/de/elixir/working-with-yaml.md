---
title:                "Arbeiten mit YAML"
html_title:           "Elixir: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Wenn du mit Konfigurationsdateien arbeitest, musst du möglicherweise auch mit YAML umgehen. YAML ist eine einfache und benutzerfreundliche Syntax, um Daten zu strukturieren und zu speichern. Mit Elixir kannst du problemlos mit YAML-Dateien arbeiten und sie in deiner Anwendung verwenden.

## Wie man mit YAML in Elixir arbeitet

Um mit YAML in Elixir zu arbeiten, musst du zuerst das Paket "YAML" installieren. Öffne dazu deine Elixir-Projektkonfiguration und füge das folgende Paket hinzu:

```Elixir
{:yaml, "~> 0.0.2"}
```

Dann führe `mix deps.get` aus, um die Abhängigkeit zu installieren. Nun kannst du das `YAML`-Modul in deinem Code importieren:

```Elixir
import YAML
```

Um eine YAML-Datei in Elixir zu lesen, verwende die `YAML.load_file/1` Funktion und gib den Pfad zur Datei an:

```Elixir
config = YAML.load_file("config.yml")
```

Du kannst nun auf die Werte in der YAML-Datei zugreifen, indem du den entsprechenden Schlüssel verwendest:

```Elixir
config[:database][:username]
```

Um eine YAML-Datei zu schreiben, verwende die `YAML.dump/1` Funktion und gib ein beliebiges Elixir-Datenformat an:

```Elixir
data = %{name: "Max Mustermann", age: 25}
YAML.dump(data, "profile.yml")
```

Das Ergebnis wird eine YAML-Datei sein, die wie folgt aussieht:

```YAML
name: Max Mustermann
age: 25
```

## Tiefer Einblick

YAML steht für "YAML Ain't Markup Language" und wurde entwickelt, um eine einfache und benutzerfreundliche Möglichkeit zu bieten, Daten in einer menschenlesbaren Struktur zu speichern. In Elixir wird das `YAML`-Modul verwendet, das auf YamlElixir basiert, einer leistungsstarken YAML-Bibliothek.

Mit dem `YAML`-Modul kannst du auch benutzerdefinierte Typen definieren und verwenden. Dies ermöglicht es dir, komplexere Datenstrukturen in einer YAML-Datei zu speichern, die dann direkt in Elixir eingelesen werden können. Du kannst mehr über die Verwendung von benutzerdefinierten Typen in der offiziellen Elixir-Dokumentation erfahren.

## Siehe auch

- Offizielle Elixir-Dokumentation: https://hexdocs.pm/elixir/YAML.html
- YamlElixir-Bibliothek: https://github.com/KamilLelonek/yamler
- YAML-Spezifikation: https://yaml.org/spec/1.2/spec.html