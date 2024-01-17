---
title:                "Å jobbe med json"
html_title:           "Ruby: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON står for JavaScript Object Notation og er et populært datatransmisjonsformat som brukes av programmører for å lagre og utveksle data mellom forskjellige applikasjoner. JSON er enkelt å lese og skrive, og det er et foretrukket valg for websideutviklere når de jobber med AJAX-forespørsler og API-integrasjoner.

## Slik gjør du det:
Det er enkelt å jobbe med JSON i Ruby. Vi bruker standardbiblioteket 'JSON' for å konvertere data til og fra JSON-format. Her er et eksempel på hvordan du kan opprette et JSON-objekt og konvertere det til en streng:

```ruby
require 'json'
data = {
  name: 'Ruby',
  type: 'programming language',
  year: 1995
}
json_string = JSON.generate(data)
```

For å konvertere en JSON-streng tilbake til et Ruby-objekt, kan du bruke metoden ```JSON.parse```:

```ruby
json_string = '{"name": "Ruby", "type": "programming language", "year": 1995}'
data = JSON.parse(json_string)
puts data[:name] # => Ruby
```

## Dypdykk:
JSON ble opprinnelig utviklet av Douglas Crockford i 2001 og var et alternativ til XML-formatet. Siden da har det blitt utbredt og er nå et standardformat for å utveksle data.
Det finnes flere populære alternativer til JSON, blant annet YAML og XML. Imidlertid er JSON et foretrukket valg fordi det er mer lettvektig og enklere å bruke.

Når man jobber med JSON i Ruby, vil de fleste operasjoner være basert på hashes og arrays, som er vanlige datatyper i Ruby. Dette gjør det enkelt å integrere JSON-formatet i eksisterende koder.

## Se også:
Du kan lese mer om JSON i Ruby i Ruby-dokumentasjonen: https://ruby-doc.org/stdlib-2.7.1/libdoc/json/rdoc/JSON.html
For en mer praktisk tilnærming og eksempler på hvordan du kan bruke JSON i Rails-prosjekter, kan du sjekke ut denne guiden: https://guides.rubyonrails.org/working_with_javascript_in_rails.html#json-for-data-interchange