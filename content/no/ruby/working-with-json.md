---
title:                "Arbeid med JSON"
date:                  2024-01-19
simple_title:         "Arbeid med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON står for JavaScript Object Notation. Det er et lett format for utveksling av data, og programmerere bruker det fordi det er enkelt å lese og skrive, og det er enkelt for maskiner å parse og generere.

## Hvordan gjøre det:
For å jobbe med JSON i Ruby, bruker vi `json`-biblioteket som allerede er inkludert i standardbiblioteket.

```Ruby
require 'json'

# Konvertere en hash til en JSON-streng
person = { navn: "Ola", alder: 27 }
person_json = person.to_json
puts person_json
# Output: {"navn":"Ola","alder":27}

# Parse en JSON-streng til en Ruby-hash
json_streng = '{"navn":"Kari","alder":31}'
ruby_person = JSON.parse(json_streng)
puts ruby_person["navn"]  # Output: Kari
```

## Dypdykk
JSON ble oppfunnet av Douglas Crockford tidlig på 2000-tallet. Før JSON ble populært, brukt mange XML for datautveksling. En fordel med JSON sammenlignet med XML er at det er mer konsist og enklere å forstå. 

Ruby-implementasjonen av JSON er rett frem. `#to_json` og `JSON.parse` er de to hovedmetodene vi bruker. `json`-biblioteket som er inkludert i Ruby bruker C-utvidelser for optimal ytelse.

XML, YAML, og BSON er alternativer til JSON. Hvert format har sine styrker og bruksområder. YAML blir ofte brukt for konfigurasjonsfiler, mens BSON er en binær versjon av JSON som ofte brukes sammen med MongoDB.

## Se også
Her er noen linker til videre lesing og ressurser:
- Offisiell Ruby JSON-dokumentasjon: [https://ruby-doc.org/stdlib-2.6.1/libdoc/json/rdoc/JSON.html](https://ruby-doc.org/stdlib-2.6.1/libdoc/json/rdoc/JSON.html)
- JSON vs. XML: [https://www.w3schools.com/js/js_json_xml.asp](https://www.w3schools.com/js/js_json_xml.asp)
- YAML offisielle nettsted: [https://yaml.org/](https://yaml.org/)
- BSON spesifikasjon: [http://bsonspec.org/](http://bsonspec.org/)
