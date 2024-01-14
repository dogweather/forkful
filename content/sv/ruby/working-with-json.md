---
title:                "Ruby: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Varför 

Att arbeta med JSON i Ruby kan vara en viktig färdighet för utvecklare, eftersom JSON är ett vanligt format som används för att utbyta data mellan program. Det är också ett lämpligt format för webbapplikationer och API:er. Genom att lära sig hur man arbetar med JSON i Ruby kan du effektivt hantera datautbytet mellan olika system. 

## Hur man gör det 

För att hantera JSON i Ruby, behöver du först installera RubyGems-paketet för "JSON". Om du använder RubyGems kan du installera det genom att köra följande kommando från terminalen: 

```Ruby 
gem install json 
``` 

När installationen är klar kan du använda biblioteket genom att använda kommandon som `parse` eller `load`, exempelvis: 

```Ruby 
require 'json'

json_str = '{"namn": "Alice", "ålder": 25}' 
data = JSON.parse(json_str) 

puts data["namn"] #=> "Alice"
puts data["ålder"] #=> 25
``` 

Det finns också andra metoder som `generate` för att konvertera Ruby-objekt till JSON-strängar och `pretty_generate` för att få en läsbar utskrift av dina JSON-data. 

## Djupdykning 

För att förstå mer om hur man arbetar med JSON i Ruby är det viktigt att ha en grundläggande förståelse för JSON-formatet. JSON står för "JavaScript Object Notation" och är en sträng med data i key-value-format som är enkelt att läsa för både människor och datorer. Det består av samlingar av par av sinsemellan unika nycklar och värden. 

Ruby har inbyggda metoder för att konvertera JSON-data till och från Ruby-objekt. Men för att göra detta måste JSON-strängen vara korrekt formaterad och giltig. Om du har problem med att hantera JSON i Ruby, är det alltid bra att kontrollera att din sträng är giltig enligt JSON-specifikationen. 

## Se också 

- [Officiell dokumentation för JSON Ruby biblioteket](https://ruby-doc.org/stdlib-2.6.1/libdoc/json/rdoc/JSON.html) 
- [Introduktion till JSON av Mozilla](https://developer.mozilla.org/sv/docs/Learn/JavaScript/Objects/JSON)