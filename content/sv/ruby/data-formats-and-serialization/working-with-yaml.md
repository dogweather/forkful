---
title:                "Att Arbeta med YAML"
aliases:
- /sv/ruby/working-with-yaml/
date:                  2024-02-03T19:26:30.853232-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
YAML, som står för YAML Ain't Markup Language, används flitigt i Ruby för konfigurationsfiler och data-serialisering på grund av sitt lättlästa format. Programmerare vänder sig till YAML när de behöver lagra eller överföra dataobjekt på ett läsligt men strukturerat sätt, vilket förenklar uppgifter som konfigurationshantering, datalagring och datadelning mellan språk.

## Hur man gör:
Ruby kommer med ett inbyggt bibliotek som heter Psych för att tolka och generera YAML. För att använda det måste du först kräva YAML standardbiblioteket. Här är ett enkelt exempel för att komma igång:

```ruby
require 'yaml'

# Hash som ska serialiseras
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Omgör hashen till YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Exempelutdata:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

För att ladda YAML-data tillbaka till ett Ruby-objekt:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Exempelutdata:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Användning av tredjepartsbibliotek:

Även om standardbiblioteket är tillräckligt för grundläggande uppgifter kan du för mer komplexa behov undersöka tredjeparts gems som 'safe_yaml'. För att använda sådana bibliotek måste du först installera gemet:

```bash
gem install safe_yaml
```

Sedan kan du använda det för att säkert ladda YAML-data, vilket minskar risker som objektinstansiering från användarkontrollerade källor:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Exempelutdata:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Detta tillvägagångssätt ökar säkerheten i din YAML-hantering, vilket gör det till ett bra val för applikationer som laddar YAML från opålitliga källor.
