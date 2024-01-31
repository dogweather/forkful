---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML (YAML Ain't Markup Language) ist ein leicht lesbares Format für Daten. Ruby-Programmierer nutzen YAML oft für Konfigurationsdateien oder zum Auslesen und Schreiben komplexer Datenstrukturen, da es einfacher als XML oder JSON ist und gut mit Ruby's eingebauter YAML-Bibliothek funktioniert.

## How to:
Installiere die YAML-Bibliothek, wenn noch nicht geschehen:
```Ruby
require 'yaml'
```

YAML-Daten in Ruby-Objekte umwandeln:
```Ruby
yaml_string = "
- Hase
- Fuchs
- Igel
"
ruby_array = YAML.load(yaml_string)
puts ruby_array.inspect  # Ausgabe: ["Hase", "Fuchs", "Igel"]
```

Ruby-Objekte in YAML umwandeln:
```Ruby
ruby_hashtable = {'name' => 'Tom', 'tier' => 'Katze'}
yaml_data = ruby_hashtable.to_yaml
puts yaml_data
# Ausgabe:
# ---
# name: Tom
# tier: Katze
```

Einlesen von YAML-Dateien:
```Ruby
file_content = YAML.load_file('pfad/zu/deiner/datei.yml')
puts file_content.inspect  # Zeigt dir den Inhalt der YAML-Datei
```

Schreibe Daten in eine YAML-Datei:
```Ruby
File.open('pfad/zu/deiner/datei.yml', 'w') do |file|
  file.write(ruby_hashtable.to_yaml)
end
```

## Deep Dive
YAML startete 2001 und ist besonders in der Ruby-Community beliebt. Alternativen wie JSON oder XML sind auch verbreitet, aber YAML’s Stärke liegt in der Lesbarkeit und Einfachheit. Bei der internen Implementierung verwendet Ruby's YAML-Modul Psych, eine Ruby-Bibliothek, die in C geschrieben ist und auf libyaml aufbaut.

## See Also
- Die offizielle YAML-Website: [https://yaml.org](https://yaml.org)
- libyaml, die YAML Parser and Emitter Bibliothek: [https://github.com/yaml/libyaml](https://github.com/yaml/libyaml)
