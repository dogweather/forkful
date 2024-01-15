---
title:                "Jobber med yaml"
html_title:           "Ruby: Jobber med yaml"
simple_title:         "Jobber med yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med YAML kan være svært nyttig for utviklere, enten man jobber med webapplikasjoner eller baksystemer. YAML er et tekstbasert format som gjør det enkelt å strukturere og lagre data, og det brukes ofte til konfigurasjonsfiler og informasjonsutveksling mellom programmer.

## Slik gjør du det

For å kunne jobbe med YAML i Ruby, må man først installere en YAML-pakke. Dette kan gjøres ved å kjøre kommandoen `gem install yaml`. Deretter kan man begynne å bruke YAML-funksjoner i Ruby-kode.

```ruby
require 'yaml'

# Lager en YAML-fil
data = { name: "Lena", city: "Oslo" }

File.open("person.yml", "w") { |f| f.write(data.to_yaml) }

# Leser data fra YAML-filen
person = YAML.load_file("person.yml")

puts person[:name] # Output: Lena
```

```ruby
# Konverterer YAML til JSON
require 'yaml'
require 'json'

yaml_data = { name: "Maria", city: "Trondheim" }

json_data = yaml_data.to_json

puts json_data # Output: {"name":"Maria","city":"Trondheim"}
```

## Dykk dypere

YAML står for "YAML Ain't Markup Language" og er et lettleselig dataserialiseringsspråk. Det er basert på nøkkelen-verdien-parentes-mønsteret som også brukes i XML og JSON. YAML har en rekke nyttige funksjoner, som for eksempel muligheten til å inkludere andre YAML-dokumenter, definere egne datatyper og kommentere kode. Dette gjør det til et kraftig verktøy for å organisere og lagre data i programmeringsprosjekter.

## Se også

- Offisiell YAML-nettside: https://yaml.org/
- Ruby YAML-dokumentasjon: https://ruby-doc.org/stdlib-2.7.2/libdoc/yaml/rdoc/YAML.html
- Yamlrb – en populær YAML-pakke for Ruby: https://github.com/alesguzik/yamlrb