---
title:                "Ruby: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

Varför: YAML är en förenklad språkstruktur för att representera data i ett human-readable format. Det är användbart för att överföra data mellan system och för att konfigurera program och webbsidor.

Hur man gör det: Användningen av YAML i Ruby är enkelt med hjälp av "YAML" biblioteket. Först måste du ladda in biblioteket genom att skriva "require 'yaml'" i din kod. Sedan kan du läsa in YAML data med "YAML.load_file" funktionen och spara det i en variabel. Du kan även skriva YAML data till en fil med "YAML.dump" funktionen. Här är ett exempel på hur man skulle läsa in en YAML fil och skriva ut den i konsolen:

```Ruby
require 'yaml'
data = YAML.load_file("example.yml")
puts data
```

Det här kommer att ge dig ett human-readable format av data från filen "example.yml". Om du vill lägga till ny data till din YAML fil, kan du använda "YAML.dump" funktionen. Till exempel, om du vill lägga till ett nytt element i din fil, kan du använda följande kod:

```Ruby
require 'yaml'
data = YAML.load_file("example.yml")
data["nyckel"] = "värde"
File.open("example.yml", 'w') {|f| f.write YAML.dump(data) }
```

Det här kommer att lägga till ett nytt element med nyckeln "nyckel" och värdet "värde" till din YAML fil.

Djupdykning: YAML tillåter också att man använder variabler och strukturer för att organisera data. Till exempel kan du skapa en struktur i din YAML fil som följande:

```yml
hem:
  plats: Stockholm
  adress: Drottninggatan 5
  postnummer: 111 21
```
Dessa variabler kan sedan användas i din kod för att hämta data. Här är ett exempel på hur du skulle kunna hämta adressen från din YAML fil:

```Ruby
require 'yaml'
data = YAML.load_file("example.yml")
hemadress = data["hem"]["adress"]
puts hemadress
```

Detta skulle skriva ut "Drottninggatan 5" i konsolen baserat på informationen från din YAML fil.

Se även: För mer information om hur man arbetar med YAML i Ruby, se följande länkar:

- YAML dokumentation: https://yaml.org/
- Ruby YAML bibliotek: https://ruby-doc.org/stdlib-2.7.2/libdoc/yaml/rdoc/YAML.html