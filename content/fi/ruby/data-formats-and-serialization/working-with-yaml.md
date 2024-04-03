---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:24.538004-07:00
description: "YAML, joka tarkoittaa YAML Ain't Markup Language, on laajalti k\xE4\
  yt\xF6ss\xE4 Rubyn konfiguraatiotiedostoissa ja datan sarjallistamisessa sen ihmislukuisan\u2026"
lastmod: '2024-03-13T22:44:57.108207-06:00'
model: gpt-4-0125-preview
summary: "YAML, joka tarkoittaa YAML Ain't Markup Language, on laajalti k\xE4yt\xF6\
  ss\xE4 Rubyn konfiguraatiotiedostoissa ja datan sarjallistamisessa sen ihmislukuisan\
  \ muodon vuoksi."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Mikä ja miksi?
YAML, joka tarkoittaa YAML Ain't Markup Language, on laajalti käytössä Rubyn konfiguraatiotiedostoissa ja datan sarjallistamisessa sen ihmislukuisan muodon vuoksi. Ohjelmoijat suosivat YAMLia, kun heidän tarvitsee tallentaa tai siirtää dataobjekteja lukuisassa mutta rakenteellisesti jäsennellyssä muodossa, mikä yksinkertaistaa tehtäviä kuten konfiguraation hallinta, datan tallennus ja kielten välisen datan jakaminen.

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
