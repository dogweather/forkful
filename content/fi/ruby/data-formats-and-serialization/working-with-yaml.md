---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:24.538004-07:00
description: "Kuinka: Ruby sis\xE4lt\xE4\xE4 sis\xE4\xE4nrakennetun kirjaston nimelt\xE4\
  \ Psych YAMLin j\xE4sennyst\xE4 ja tuottamista varten. K\xE4ytt\xE4\xE4ksesi sit\xE4\
  , sinun t\xE4ytyy ensin vaatia YAML-\u2026"
lastmod: '2024-03-13T22:44:57.108207-06:00'
model: gpt-4-0125-preview
summary: "Ruby sis\xE4lt\xE4\xE4 sis\xE4\xE4nrakennetun kirjaston nimelt\xE4 Psych\
  \ YAMLin j\xE4sennyst\xE4 ja tuottamista varten."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Kuinka:
Ruby sisältää sisäänrakennetun kirjaston nimeltä Psych YAMLin jäsennystä ja tuottamista varten. Käyttääksesi sitä, sinun täytyy ensin vaatia YAML-standardikirjastoa. Tässä on yksinkertainen esimerkki aloittamiseen:

```ruby
require 'yaml'

# Sarjallistettava hajautustaulu
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Hajautustaulun muuntaminen YAMLiksi
yaml_data = person.to_yaml

puts yaml_data
```

**Esimerkkituloste:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Ladataksesi YAML-datan takaisin Ruby-objektiksi:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Esimerkkituloste:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Kolmannen osapuolen kirjastojen käyttö:
Vaikka standardikirjasto riittää perustehtäviin, monimutkaisiin tarpeisiin saattaisit haluta tutkia kolmannen osapuolen jalokiviä kuten 'safe_yaml'. Käyttääksesi tällaisia kirjastoja, sinun täytyy ensin asentaa jalokivi:

```bash
gem install safe_yaml
```

Sen jälkeen voit käyttää sitä turvallisesti ladatakseen YAML-dataa, vähentäen riskejä kuten objektien instanssiointi käyttäjän hallitsemista lähteistä:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Esimerkkituloste:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Tämä lähestymistapa parantaa YAML-käsittelysi turvallisuutta, tehden siitä hyvän valinnan sovelluksille, jotka lataavat YAML-dataa luottamattomista lähteistä.
