---
title:                "Att arbeta med json"
html_title:           "Ruby: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON är ett format för att strukturera och överföra data i läsbart format, som används av många moderna webbapplikationer. Programmörer använder JSON för att effektivt hämta och skicka data från en server, för att bygga dynamiska och interaktiva webbsidor.

## Hur man:
För att arbeta med JSON i Ruby, behöver du först importera JSON-biblioteket genom att skriva `require 'json'` i din kod. Sedan kan du använda `JSON.parse()` för att läsa in JSON-data och `JSON.generate()` för att omvandla Ruby-data till JSON-format.

```Ruby
require 'json'

# Läsa in JSON-data från ett API-samtal
response = '{"name": "Jane", "age": 25, "city": "Stockholm"}'
data = JSON.parse(response)

puts data['name'] # => "Jane"
puts data['age'] # => 25
puts data['city'] # => "Stockholm"

# Konvertera Ruby-data till JSON-format
person = {name: "John", age: 30, city: "Göteborg"}
json_data = JSON.generate(person)

puts json_data # => '{"name":"John","age":30,"city":"Göteborg"}'
```

## Deep Dive:
JSON, eller JavaScript Object Notation, utvecklades först av Douglas Crockford på 1990-talet som ett enklare dataformat för webben jämfört med XML. Det används ofta för att överföra data mellan en webbtjänst och en client-applikation, och har blivit standard för många moderna webbapplikationer.

Det finns olika alternativ till JSON för dataöverföring, som till exempel XML, YAML och CSV. Men JSON har blivit populärt på grund av dess enkelhet, läsbarhet och förmågan att hantera komplexa datastrukturer.

När det gäller implementation är JSON-integration inbyggd i Ruby, vilket gör det enkelt att arbeta med JSON-data i dina projekt. Ruby stöder också olika metoder för att konvertera mellan JSON och Ruby-objekt, beroende på dina behov.

## Se även:
- [Ruby JSON-dokumentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html)
- [An Introduction to JSON and Ruby](https://www.sitepoint.com/an-introduction-to-json-and-ruby/)
- [Why JSON has become the lingua franca for data exchange](https://www.infoworld.com/article/3513308/why-json-has-become-the-lingua-franca-for-data-exchange.html)