---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:54.301462-07:00
description: "Hur man g\xF6r: Att skapa och anv\xE4nda hashar i Ruby \xE4r enkelt.\
  \ Du kan initialisera en tom hash, fylla den med nyckel-v\xE4rdepar, komma \xE5\
  t v\xE4rden via deras\u2026"
lastmod: '2024-03-13T22:44:38.421555-06:00'
model: gpt-4-0125-preview
summary: "Att skapa och anv\xE4nda hashar i Ruby \xE4r enkelt."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
Att skapa och använda hashar i Ruby är enkelt. Du kan initialisera en tom hash, fylla den med nyckel-värdepar, komma åt värden via deras nycklar och mer. Så här gör du det:

```Ruby
# Skapa en hash
my_hash = { "name" => "John Doe", "age" => 30 }

# Ett annat sätt att skapa en hash
another_hash = Hash.new
another_hash["position"] = "Utvecklare"

# Komma åt hashvärden
puts my_hash["name"] # Utdata: John Doe

# Lägga till ett nytt nyckel-värdepar
my_hash["språk"] = "Ruby"
puts my_hash # Utdata: {"name"=>"John Doe", "age"=>30, "språk"=>"Ruby"}

# Iterera genom en hash
my_hash.each do |nyckel, värde|
  puts "#{nyckel}: #{värde}"
end
# Utdata:
# namn: John Doe
# ålder: 30
# språk: Ruby
```

Du kan också använda symboler som effektivare nycklar:

```Ruby
# Använda symboler för nycklar
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # Utdata: Jane Doe
```

## Fördjupning:
Konceptet med associativa arrayer är inte unikt för Ruby; många språk implementerar dem under olika namn, som dictionaries i Python eller objekt i JavaScript (när de används som nyckel-värdepar). I början av Rubys historia var hashar något långsammare och inte lika mångsidiga. Men över tid har Rubys implementering av hashar blivit mycket optimerad, särskilt för symbolnycklar, vilket gör dem extremt effektiva för frekvent tillgång och uppdateringar.

Ruby-hashar utmärker sig för sin syntaktiska enkelhet och flexibilitet - du kan använda nästan vilken objekttyp som helst som en nyckel, även om symboler och strängar är vanligast. Internt implementeras Ruby-hashar med hjälp av en hashningsalgoritm som balanserar hastighet och minneseffektivitet, även när antalet element ökar.

Även om hashar är otroligt mångsidiga, är de inte den ultimata lösningen för datalagring i Ruby. För ordnade samlingar är arrayer mer lämpliga, och för mängder av unika objekt kan ett Set vara ett bättre val. Dessutom, för mycket komplexa datastrukturer, kan det vara lämpligt att skapa anpassade klasser.

Kom ihåg, valet av att använda en hash jämfört med andra datastrukturer beror till stor del på specifikt användningsfall – hashar är utmärkta för snabba uppslag och upprätthållande av associationer mellan unika nycklar och deras värden.
