---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:30.853232-07:00
description: "Hur man g\xF6r: Ruby kommer med ett inbyggt bibliotek som heter Psych\
  \ f\xF6r att tolka och generera YAML. F\xF6r att anv\xE4nda det m\xE5ste du f\xF6\
  rst kr\xE4va YAML\u2026"
lastmod: '2024-03-13T22:44:38.452156-06:00'
model: gpt-4-0125-preview
summary: "Ruby kommer med ett inbyggt bibliotek som heter Psych f\xF6r att tolka och\
  \ generera YAML."
title: Att Arbeta med YAML
weight: 41
---

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
