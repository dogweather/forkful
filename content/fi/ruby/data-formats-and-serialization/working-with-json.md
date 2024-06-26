---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:16.343170-07:00
description: "Miten: Ruby tarjoaa standardikirjastonsa avulla saumattomia tapoja j\xE4\
  sent\xE4\xE4 ja tuottaa JSON-muotoista dataa. P\xE4\xE4moduuli n\xE4ille toiminnoille\
  \ on `json`,\u2026"
lastmod: '2024-03-13T22:44:57.109304-06:00'
model: gpt-4-0125-preview
summary: "Ruby tarjoaa standardikirjastonsa avulla saumattomia tapoja j\xE4sent\xE4\
  \xE4 ja tuottaa JSON-muotoista dataa."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Miten:
Ruby tarjoaa standardikirjastonsa avulla saumattomia tapoja jäsentää ja tuottaa JSON-muotoista dataa. Päämoduuli näille toiminnoille on `json`, joka voidaan helposti integroida mihin tahansa Ruby-sovellukseen.

### JSON:n jäsentäminen:
JSON-merkkijonon muuntamiseksi Ruby-hashiksi voit käyttää `JSON.parse` -metodia.

```ruby
require 'json'

json_merkkijono = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_merkkijono)

puts ruby_hash
# Tuloste: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### JSON:n tuottaminen:
Vastaavasti, kun haluat muuntaa Ruby-hashin JSON-merkkijonoksi, käytät `JSON.generate` -metodia tai `to_json` -metodia, joka on käytettävissä Ruby-objekteilla sen jälkeen, kun `json`-kirjasto on vaadittu.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_merkkijono = ruby_hash.to_json

puts json_merkkijono
# Tuloste: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Kolmannen osapuolen kirjastot:
Vaikka Rubyn standardikirjasto kattaa perus JSON-käsittelyn, monet projektit nojaavat kolmannen osapuolen kirjastoihin parannetun toiminnallisuuden ja suorituskyvyn vuoksi. Yksi suosittu vaihtoehto on `Oj` (Optimized JSON).

#### Jäsentäminen Oj:n avulla:
```ruby
require 'oj'

json_merkkijono = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_merkkijono)

puts ruby_hash
# Tuloste: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Tuottaminen Oj:n avulla:
Oj tarjoaa myös nopean tavan tuottaa JSON-muotoista dataa Ruby-objekteista:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_merkkijono = Oj.dump(ruby_hash)

puts json_merkkijono
# Tuloste: {"name":"Samantha","age":35,"city":"Miami"}
```

Nämä esimerkit havainnollistavat JSON:n kanssa työskentelyn suoraviivaista luonnetta Rubyssa, tehden siitä saavutettavan tehtäville, jotka vaihtelevat yksinkertaisista datan manipuloinneista monimutkaisiin API-viestintöihin.
