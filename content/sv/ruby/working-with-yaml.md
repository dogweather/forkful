---
title:                "Arbete med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML, "YAML Ain't Markup Language", hanterar konfigurationer och datautbyte. För Ruby-utvecklare är det en no-brainer för läslig datastrukturering och smidig integration med Rails.

## Hur gör man:
Installera YAML-biblioteket:
```ruby
gem install 'yaml'
```

Läs en YAML-fil:
```ruby
require 'yaml'

config = YAML.load_file('exempel.yaml')
puts config.inspect
```

Skapa och skriv till en YAML-fil:
```ruby
require 'yaml'

data = { namn: 'Erik', yrke: 'Utvecklare', språk: ['Ruby', 'JavaScript'] }
File.write('ny_exempel.yaml', data.to_yaml)
```

Resultat:
```
---
:namn: Erik
:yrke: Utvecklare
:språk:
- Ruby
- JavaScript
```

## Deep Dive
YAML debuterade 2001, erbjuder bättre läsbarhet än XML. Alternativ inkluderar JSON och TOML. Ruby's YAML-bibliotek, Psych, binder till libyaml och är standard sedan Ruby 1.9.3.

## Se också
- YAML officiell specifikation: https://yaml.org/spec/1.2/spec.html
- Ruby's Psych dokumentation: https://ruby-doc.org/stdlib-2.5.1/libdoc/psych/rdoc/Psych.html
- Ruby on Rails konfigurationsguide: https://guides.rubyonrails.org/configuring.html
