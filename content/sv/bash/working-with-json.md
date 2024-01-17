---
title:                "Arbeta med json"
html_title:           "Bash: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Vad och Varför?
JSON (JavaScript Object Notation) är en lättläst, humanvänlig strukturerad dataformat som är vanligt används för att skicka och lagra data i webbapplikationer. Genom att använda JSON kan programmerare lätt konvertera data från en form till en annan, och dela information mellan olika plattformar. 

## Så här gör du:
Bash stöder inte inbyggt behandling av JSON-data, men det finns verktyg som underlättar denna uppgift. Det enklaste sättet att arbeta med JSON i Bash är att använda kommandolinjeverktyget `jq`. Detta verktyg tillåter programmerare att lätt more player data. 

```Bash
# Först ladda ner och installera jq
sudo apt-get install jq

# Skapa ett exempel-JSON-fil
{
    "namn": "Anna",
    "ålder": 25,
    "favoritfärg": "blå"
}

# Läs data från JSON-filen
cat filnamn.json

# Använd jq för att hämta ett specifikt värde
jq '.ålder' filnamn.json
```

Output:

```Bash
25
```

## Djupdykning:
JSON uppfanns 2001 som en alternativ till XML för att lagra och distribuera data på webben. Sedan dess har det blivit en av de mest använda filformaten för att utbyta data mellan applikationer. Det finns också andra alternativ som XML och YAML, men JSON är vanligtvis enklare och mer lättläst.

Förutom jq, finns det andra Bash-verktyg som kan hjälpa till att bearbeta och manipulera JSON-data, till exempel `jshon` och `yq`. Men ibland kan det vara mer gynnsamt att använda ett annat programmeringsspråk som har inbyggda funktioner för att hantera JSON, som t.ex. Python eller Node.js.

## Se även:
- [jq dokumentation](https://stedolan.github.io/jq/)
- [Alternativ till JSON](https://dev.to/snjh/alternative-to-json-58da)