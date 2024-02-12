---
title:                "Työskentely YAML:n kanssa"
date:                  2024-02-03T19:26:24.538004-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
