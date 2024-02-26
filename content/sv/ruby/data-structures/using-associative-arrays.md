---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:54.301462-07:00
description: "Associativa arrayer, som oftare kallas hashar i Ruby, g\xF6r det m\xF6\
  jligt att para ihop unika nycklar med v\xE4rden. De \xE4r oumb\xE4rliga n\xE4r du\
  \ beh\xF6ver h\xE5lla reda\u2026"
lastmod: '2024-02-25T18:49:36.736281-07:00'
model: gpt-4-0125-preview
summary: "Associativa arrayer, som oftare kallas hashar i Ruby, g\xF6r det m\xF6jligt\
  \ att para ihop unika nycklar med v\xE4rden. De \xE4r oumb\xE4rliga n\xE4r du beh\xF6\
  ver h\xE5lla reda\u2026"
title: "Att anv\xE4nda associativa arrayer"
---

{{< edit_this_page >}}

## Vad & Varför?

Associativa arrayer, som oftare kallas hashar i Ruby, gör det möjligt att para ihop unika nycklar med värden. De är oumbärliga när du behöver hålla reda på element genom en specifik referens, som att lagra egenskaperna hos ett objekt eller snabbt få tillgång till data via en unik identifierare.

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
