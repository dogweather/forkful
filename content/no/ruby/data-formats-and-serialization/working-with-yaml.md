---
title:                "Arbeider med YAML"
date:                  2024-02-03T19:26:30.065636-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeider med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML, som står for YAML Ain't Markup Language, er mye brukt i Ruby for konfigurasjonsfiler og dataserialisering på grunn av sitt lesbare format. Programmerere trekkes mot YAML når de trenger å lagre eller overføre dataobjekter på en lesbar, men strukturert måte, noe som forenkler oppgaver som konfigurasjonsstyring, datalagring og deling av data mellom språk.

## Hvordan:
Ruby kommer med et innebygget bibliotek kalt Psych for parsing og utforming av YAML. For å bruke det, må du først kreve YAML-standardbiblioteket. Her er et grunnleggende eksempel for å komme i gang:

```ruby
require 'yaml'

# Hash som skal serialiseres
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Konvertere hashen til YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Eksempel På Output:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

For å laste YAML-data tilbake til et Ruby-objekt:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Eksempel På Output:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Bruke Tredjepartsbiblioteker:

Selv om standardbiblioteket er tilstrekkelig for grunnleggende oppgaver, for mer komplekse behov kan du se etter tredjepartsgems som 'safe_yaml'. For å bruke slike biblioteker, må du først installere gemen:

```bash
gem install safe_yaml
```

Deretter kan du bruke den til å sikkert laste YAML-data, noe som reduserer risikoer som objektinstansiering fra brukerkontrollerte kilder:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Eksempel På Output:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Denne tilnærmingen forbedrer sikkerheten til din YAML-håndtering, noe som gjør den til et godt valg for applikasjoner som laster YAML fra ikke-pålitelige kilder.
