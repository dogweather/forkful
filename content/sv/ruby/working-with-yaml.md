---
title:                "Att arbeta med yaml"
html_title:           "Ruby: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Arbetar du med Ruby, så är chansen stor att du stött på YAML. YAML (YAML Ain't Markup Language) är ett strukturerat textformat som används för att representera data. Det är ett populärt val bland programmerare på grund av dess enkelhet och läsbarhet.

## Hur man gör:
```Ruby
# Skapa en YAML-fil
yaml_data = { name: "Lisa", age: 25, occupation: "webbutvecklare" }
File.open("min_fil.yml", "w") do |file|
  file.write(yaml_data.to_yaml)
end

# Läsa och bearbeta en YAML-fil
require 'yaml'
data = YAML.load_file("min_fil.yml")
puts "Namn: #{data[:name]}"
puts "Ålder: #{data[:age]}"
puts "Yrke: #{data[:occupation]}"

# Exempel på output:
# Namn: Lisa
# Ålder: 25
# Yrke: webbutvecklare
```

## Deep Dive:
YAML utvecklades av Clark Evans under början av 2000-talet och har sedan dess blivit en populär standard för att läsa och skriva datastrukturer i textformat. Ett vanligt alternativ till YAML är JSON, men YAML anses vara mer människoläsbar med sina utrymmen och avskiljare i stället för klammerparenteser. Det finns också ett antal tredjepartsbibliotek för att arbeta med YAML i Ruby, såsom [Psych](https://github.com/ruby/psych) och [YAML](https://github.com/ruby/yaml).

## Se även:
- [YAML Specifikation](https://yaml.org/spec/)
- [Officiell YAML-sida](https://yaml.org/)
- [Ruby YAML-dokumentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html)
- [YAML vs JSON: Vilket är bättre för konfigurationsfiler?](https://blog.bugsnag.com/yaml-vs-json/)