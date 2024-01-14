---
title:                "Ruby: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit YAML beschäftigen? Nun, YAML ist eine einfache und benutzerfreundliche Möglichkeiten, Daten in einem strukturierten Format zu speichern und zu übertragen. Es ist besonders nützlich für Programmierer, die große Datensätze verarbeiten und austauschen müssen.

## Wie man es macht

Der erste Schritt, um mit YAML zu arbeiten, ist die Installation der YAML-Gem auf Ihrem Computer. Mit Ruby können Sie dies einfach mit dem Befehl `gem install yaml` tun.

Als nächstes können Sie mit YAML-Dokumenten arbeiten, indem Sie sie als eine Ruby-Hash-Struktur laden. Zum Beispiel:

```Ruby 
require 'yaml'
 
# Laden Sie das YAML-Dokument als eine Hash-Strukur.
data = YAML.load(File.read('data.yml'))

# Greifen Sie auf die Daten zu, wie Sie es mit einem normalen Hash tun würden.
puts data["Nutzername"]
```

Die `require 'yaml'` Anweisung stellt sicher, dass das YAML-Modul in Ihrem Projekt verfügbar ist. Dann wird das YAML-Dokument mit der `File.read` Funktion gelesen und in eine Hash-Struktur mit der `YAML.load` Funktion umgewandelt. Von dort aus können Sie die Daten wie gewohnt mit normalen Hash-Operationen wie `puts` oder `each` verwenden.

## Tiefergehende Informationen

Was ist, wenn Sie komplexe Datenstrukturen haben oder sogar YAML-Dateien programmatisch erstellen müssen? In solchen Fällen ist es sinnvoll, die `YAML.dump` Funktion zu verwenden, um einen hash in ein YAML-Dokument umzuwandeln. Zum Beispiel:

```Ruby
require 'yaml'

# Eine Hash-Struktur erstellen.
data = {
  "Titel" => "Mein Blogpost",
  "Autor" => "Max Mustermann",
  "Inhalt" => "Dies ist ein Beispielinhalt.",
  "Tags" => ["Ruby", "Programmieren", "YAML"]
}

# Den Hash in ein YAML-Dokument konvertieren.
yaml_data = YAML.dump(data)

# Das erzeugte YAML-Dokument ausgeben.
puts yaml_data
```

Dies wird das folgende YAML-Dokument ausgeben:

```YAML
--- 
Titel: Mein Blogpost
Autor: Max Mustermann
Inhalt: Dies ist ein Beispielinhalt.
Tags: 
  - Ruby
  - Programmieren
  - YAML
```

Mit YAML können Sie auch komplexe Datenstrukturen wie Arrays, Booleans und sogar verschachtelte Hashes speichern. Eine gute Übersicht über alle unterstützten Datentypen finden Sie in der offiziellen [YAML-Spezifikation](https://yaml.org/spec/1.2/spec.html#Representations).

## Siehe auch

- Offizielle YAML-Website: https://yaml.org/
- YAML-Spezifikation: https://yaml.org/spec/