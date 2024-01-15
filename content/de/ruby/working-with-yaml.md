---
title:                "Arbeiten mit YAML"
html_title:           "Ruby: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum

YAML ist eine einfache und intuitive Möglichkeit, Daten in einem formatierten und strukturierten Format darzustellen. Es ist besonders nützlich für die Konfiguration von Programmen oder das Lesen und Schreiben von Dateien. Wenn du in der Ruby-Programmierung tätig bist, ist es hilfreich, YAML zu beherrschen, da es ein häufig genutztes Format ist.

# Wie man YAML in Ruby verwendet

```ruby
require "yaml"

# Daten in YAML-Format speichern
data = { name: "Max Mustermann", alter: 35, hobbies: ["Lesen", "Reisen"] }
File.write("daten.yml", data.to_yaml)

# Daten aus YAML-Datei auslesen
daten = YAML.load(File.read("daten.yml"))
puts daten[:name]
# => Max Mustermann
puts daten[:alter]
# => 35
puts daten[:hobbies]
# => ["Lesen", "Reisen"]
```

# Tiefer Einblick

YAML steht für "YAML Ain't Markup Language" und wurde entwickelt, um eine menschenlesbare Alternative zu XML zu bieten. Ähnlich wie bei JSON, können in YAML Datenstrukturen wie Hashes und Arrays dargestellt werden. Eine Besonderheit von YAML ist die Verwendung von Einrückungen, um die Struktur der Daten zu definieren. Dies macht YAML besonders gut lesbar.

# Siehe auch

- [YAML-Dokumentation auf ruby-doc.org](https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html)
- [YAML-Spezifikation auf yaml.org](https://yaml.org/spec/)
- [YAML-Tutorial auf SitePoint](https://www.sitepoint.com/yaml-tutorial-beginners-guide/)