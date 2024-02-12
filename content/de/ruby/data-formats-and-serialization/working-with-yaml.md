---
title:                "Arbeiten mit YAML"
aliases:
- de/ruby/working-with-yaml.md
date:                  2024-02-03T19:26:26.134673-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
YAML, was für "YAML Ain't Markup Language" steht, wird in Ruby umfangreich für Konfigurationsdateien und die Serialisierung von Daten verwendet, aufgrund seines menschenlesbaren Formats. Programmierer neigen zu YAML, wenn sie Datenobjekte auf eine lesbare, aber strukturierte Weise speichern oder übertragen müssen, was Aufgaben wie die Konfigurationsverwaltung, Datenspeicherung und den Datenaustausch zwischen Sprachen vereinfacht.

## Wie geht das:
Ruby kommt mit einer eingebauten Bibliothek namens Psych zum Parsen und Emittieren von YAML. Um sie zu nutzen, müssen Sie zuerst die YAML-Standardbibliothek einbinden. Hier ist ein einfaches Beispiel, um Ihnen den Einstieg zu erleichtern:

```ruby
require 'yaml'

# Zu serialisierender Hash
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Umwandeln des Hashs in YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Beispielausgabe:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Um YAML-Daten wieder in ein Ruby-Objekt zu laden:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Beispielausgabe:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Verwendung von Drittanbieter-Bibliotheken:

Obwohl die Standardbibliothek für grundlegende Aufgaben ausreicht, könnten Sie für komplexere Bedürfnisse Drittanbieter-Gems wie 'safe_yaml' in Betracht ziehen. Um solche Bibliotheken zu nutzen, müssen Sie zuerst das Gem installieren:

```bash
gem install safe_yaml
```

Dann können Sie es verwenden, um YAML-Daten sicher zu laden und Risiken wie die Objektinstanziierung aus nutzerkontrollierten Quellen zu mindern:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Beispielausgabe:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Dieser Ansatz erhöht die Sicherheit Ihrer YAML-Handhabung und macht es zu einer guten Wahl für Anwendungen, die YAML aus nicht vertrauenswürdigen Quellen laden.
