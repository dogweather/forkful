---
title:                "Arbeid med YAML"
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
"YAML Ain't Markup Language" (YAML) er en data-serialiseringsformat brukt i konfigurasjonsfiler. Programmerere bruker YAML fordi det er menneskelesbart og samtidig lett å tolke av maskiner.

## How to:
For å håndtere YAML i Ruby, bruk `yaml` biblioteket. Her er et eksempel på hvordan laste en YAML-konfigurasjon.

```Ruby
require 'yaml'

# Last inn YAML fra en fil
config = YAML.load_file('config.yml')

# Tilgang til data i YAML-filen
puts config['database']['name']

# Output
# => "my_database"
```

For å skrive til en YAML-fil:

```Ruby
require 'yaml'

# En hash med konfigurasjonsdata
config_data = {
  database: {
    name: 'my_database',
    port: 5432
  }
}

# Skrive hashen til en ny YAML-fil
File.open('new_config.yml', 'w') { |file| file.write(config_data.to_yaml) }
```

## Deep Dive
YAML debuterte i 2001 som et enklere alternativ til XML. Det brukes ofte i programvareutvikling for applikasjonskonfigurasjon og dataoverføring. Alternativer til YAML inkluderer JSON og TOML, men YAML skiller seg ut med sin sterke lesbarhet. Ruby's YAML-implementering ligger i `Psych` biblioteket, som ble standard YAML-parser fra Ruby 1.9.3.

## See Also
- YAML offisielt nettsted: [yaml.org](https://yaml.org)
- Ruby-dokumentasjon for `Psych` biblioteket: [ruby-doc.org](https://ruby-doc.org/stdlib-3.0.0/libdoc/psych/rdoc/Psych.html)
