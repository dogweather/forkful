---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
YAML on ihmisen luettavissa oleva tiedostoformaatti datan esittämiseen. Ruby-ohjelmoijat käyttävät sitä konfiguraatioiden, asetusten ja muiden rakenteellisten tietojen tallennukseen, koska sen puhtaan ja yksinkertaisen syntaksin ansiosta tiedoston muokkaaminen ja ymmärtäminen on vaivatonta.

## How to:
Rubyssa YAML-tiedostojen käsittely on helppoa `yaml`-kirjaston avulla. Esimerkiksi näin:

```Ruby
require 'yaml'

# YAML-tiedoston lukeminen
config = YAML.load_file('config.yml')

# Hash-tietueena olevan datan tulostus
puts config.inspect

# YAML-tiedoston luominen
user_data = { name: 'Matti', language: 'Ruby', level: 'beginner' }
File.open('user.yml', 'w') { |file| file.write(user_data.to_yaml) }
```

Output:
```Ruby
{ "name" => "Matti", "language" => "Ruby", "level" => "beginner" }
```

## Deep Dive:
YAML, lyhennys sanoista "YAML Ain't Markup Language", on vuonna 2001 ilmestynyt ja W3C:n standardoima. Se on suunniteltu olemaan selkeämpi ja yksinkertaisempi kuin sen vaihtoehdot, kuten XML ja JSON. YAML-perusteita koodatessa tarvittaa `yaml`-kirjasto, joka on usein jo sisäänrakennettu Ruby-version mukana. Tiedostot tyypillisesti päättyvät `.yml` tai `.yaml` ja noudattavat avain-arvo -periaatetta.

## See Also:
- YAML:n virallinen sivusto: [yaml.org](https://yaml.org/)
- Ruby-dokumentaatio YAML-moduulille: [ruby-doc.org](https://ruby-doc.org/stdlib-2.5.1/libdoc/yaml/rdoc/YAML.html)
