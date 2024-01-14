---
title:                "Ruby: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du arbeider med programmeringsspråket Ruby, har du kanskje hørt om JSON. Men hva er egentlig JSON, og hvorfor bør du lære å jobbe med det? JSON står for JavaScript Object Notation og er et populært format for datautveksling. Det brukes ofte i moderne webutvikling og er en viktig del av Ruby-programmering. 

## Hvordan

For å arbeide med JSON i Ruby trenger du et bibliotek som hjelper deg med å håndtere JSON-data. Et av de mest brukte bibliotekene er "json", som er innebygd i Ruby. La oss se på et eksempel på hvordan du kan bruke dette biblioteket for å lage og jobbe med JSON-data. 

```Ruby
require 'json'

# Opprett en Ruby hash med informasjon om en person
person = { 
  navn: "Ole Olsen", 
  alder: 35, 
  by: "Oslo" 
}

# Konverter hashen til JSON-format
json_person = person.to_json 

# Skriv ut den nye JSON-strengen
puts json_person 

# Output: {"navn":"Ole Olsen","alder":35,"by":"Oslo"}
```

Her kan du se at ".to_json" konverterer hashen til en gyldig JSON-streng. Dette gjør det enklere å sende og motta data i forskjellige formater, for eksempel mellom to applikasjoner. 

## Dypdykk

Nå som du har en grunnleggende forståelse av hvordan du kan bruke JSON i Ruby, kan du utforske dypere og lære mer om de forskjellige funksjonene og metoder som er tilgjengelige. Ruby har mange nyttige metoder for å konvertere, parsere og manipulere JSON-data. Du kan også lære å validere og håndtere feil i JSON-data for å gjøre kodebasen din mer robust. Det er også verdt å undersøke hvordan du kan integrere JSON med populære web-rammeverk som Ruby on Rails eller Sinatra. 

## Se også

- [Ruby-docs: JSON bibliotek](https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html)
- [RailsGuides: Working with JSON in Rails](https://guides.rubyonrails.org/layouts_and_rendering.html#rendering-json)
- [Sintra: Using JSON](http://sinatrarb.com/intro.html#Rendering%20JSON)
- [JSON Validator](https://jsonlint.com)