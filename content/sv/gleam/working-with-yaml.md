---
title:                "Arbeta med yaml"
html_title:           "Gleam: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att arbeta med YAML, eller YAML Ain't Markup Language, är ett sätt för programmerare att strukturera och ordna data på ett läsbart och logiskt sätt. Det används ofta för att konfigurera och lagra inställningar, men kan också användas för att strukturera data och information i ett projekt.

# Hur du gör:
För att använda YAML i ditt Gleam-projekt, behöver du först importera YAML-paketet. Sedan kan du skapa en variabel och tilldela den värden från en YAML-fil genom att använda funktionen `decode` tillsammans med filnamnet och typen av data du vill ha.

```
import gleam/yaml

let settings: Map(String, String) =
    yaml.decode("settings.yml", Map(String, String))
```

För att skriva till en YAML-fil, kan du använda funktionen `encode` tillsammans med den variabel du vill skriva till filen och filnamnet.

```
let example_settings =
    Map.from_list(
        [
            ("language", "Swedish"),
            ("theme", "Dark"),
            ("notifications", "On")
        ]
    )

yaml.encode(example_settings, "new_settings.yml")
```

# Djupdykning:
YAML introducerades först 2001 som ett alternativ till XML-formatet för att strukturera data. YAML är utformat för att vara läsbart för både människor och maskiner, vilket gör det lätt att arbeta med och underhålla. Det finns också andra format för datastrukturering, såsom JSON och TOML, men YAML har fördelen av att vara mer lättförståeligt och lättare att läsa.

När du arbetar med YAML i Gleam, används biblioteket `gleam/yaml` som implementerar libyaml under huven. Detta betyder att du kan lita på att din YAML-kod kommer att vara effektiv och tillförlitlig för ditt projekt.

# Se även:
- Gleams YAML-paket: https://github.com/gleam-lang/yaml
- YAML-specifikationen: https://yaml.org/spec/
- Alternativa format för datastrukturering: JSON, TOML