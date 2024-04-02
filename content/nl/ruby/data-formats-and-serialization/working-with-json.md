---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:30.075577-07:00
description: "JSON, of JavaScript Object Notation, is een lichtgewicht gegevensuitwisselingsformaat.\
  \ Programmeurs gebruiken JSON om gegevens op te slaan en uit te\u2026"
lastmod: '2024-03-13T22:44:51.387449-06:00'
model: gpt-4-0125-preview
summary: "JSON, of JavaScript Object Notation, is een lichtgewicht gegevensuitwisselingsformaat.\
  \ Programmeurs gebruiken JSON om gegevens op te slaan en uit te\u2026"
title: Werken met JSON
weight: 38
---

## Wat & Waarom?
JSON, of JavaScript Object Notation, is een lichtgewicht gegevensuitwisselingsformaat. Programmeurs gebruiken JSON om gegevens op te slaan en uit te wisselen omdat het makkelijk te lezen en te schrijven is voor mensen, en eenvoudig te ontleden voor machines.

## Hoe doe je dat:
In Ruby kun je met JSON werken met behulp van de ingebouwde 'json'-bibliotheek. Om het te gebruiken, moet je gewoon 'json' bovenaan je code vereisen.

```Ruby
require 'json'

# Een Ruby hash converteren naar een JSON string
user = { name: "John Doe", email: "john.doe@example.com" }
json_string = user.to_json
puts json_string
# Output: {"name":"John Doe","email":"john.doe@example.com"}

# Een JSON string ontleden naar een Ruby hash
json_string = '{"name":"Jane Doe","email":"jane.doe@example.com"}'
parsed_data = JSON.parse(json_string)
puts parsed_data["name"]
# Output: Jane Doe
```

## Diepgaand
JSON vindt zijn oorsprong in de vroege jaren 2000. Douglas Crockford, de promotor ervan, zocht naar een manier om gegevensdeling tussen server en cliënt in webapplicaties eenvoudiger te maken in vergelijking met XML.

Alternatieven voor JSON zijn onder andere XML en YAML, hoewel het gebruiksgemak van JSON en de wijdverbreide ondersteuning ervan het een voorkeursformaat maken. Het ontleden van JSON in Ruby is efficiënt omdat de 'json'-bibliotheek is gebouwd op native extensies geschreven in C, wat het ontleden aanzienlijk versnelt.

## Zie ook
- JSON specificaties en info site: [JSON.org](https://www.json.org/json-en.html)
- Vergelijking van JSON en XML: [XML vs JSON](https://www.w3schools.com/js/js_json_xml.asp)
